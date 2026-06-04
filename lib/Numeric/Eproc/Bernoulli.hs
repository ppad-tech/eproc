{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.Bernoulli
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- One-sided Bernoulli rate anytime-valid test.
--
-- For samples @x_t@ in @{0, 1}@, tests @H_0: E[x] <= p_0@ against
-- @H_1: E[x] > p_0@.
--
-- A single wealth process is run:
--
--     @W_n = prod_{i=1..n} (1 + lambda_i * (x_i - p_0))@
--
-- where each per-step bet @lambda_i@ is chosen predictably (from
-- data observed strictly before step @i@) and clipped to
-- @[0, lambda_max]@ so that the wealth factor stays nonnegative for
-- every admissible observation. Under @H_0@ the wealth process is
-- a nonnegative supermartingale, so by Ville's inequality the
-- probability of @W_n@ ever crossing @1 \/ alpha@ is at most
-- @alpha@, regardless of when the user decides to stop streaming
-- samples.
--
-- Unlike "Numeric.Eproc.Bounded", the alternative here is one-sided,
-- so a single wealth process suffices and no Bonferroni adjustment
-- is needed -- the rejection threshold is @log(1 \/ alpha)@.
--
-- == Example
--
-- Test @H_0: E[x] <= 0.05@ at level @alpha = 1e-3@ against a stream
-- with empirical rate @~0.5@:
--
-- >>> let cfg = config 1.0e-3 0.05 Newton
-- >>> let xs  = take 200 (cycle [True, False])
-- >>> decide cfg (foldl' (update cfg) (initial cfg) xs)
-- Reject

module Numeric.Eproc.Bernoulli (
  -- * Test configuration and state
    Config
  , State
  , Verdict(..)

  -- * Bettor strategies
  , Bettor(..)

  -- * Construction
  , config
  , initial

  -- * Streaming
  , update
  , decide

  -- * Inspection
  , log_wealth
  , samples
  ) where

-- types ----------------------------------------------------------------------

-- | A predictable bettor.
--
--   A bettor describes how, given the history of centred observations
--   @z_t = x_t - p_0@, the next predictable bet @lambda_t@ is chosen.
--   Predictability -- that is, @lambda_t@ depends only on data
--   observed strictly before step @t@ -- is what makes the resulting
--   wealth process a nonnegative supermartingale under @H_0@.
--
--   For 'Adaptive' and 'Newton', a safe-bet ceiling @lambda_max@ is
--   derived from the baseline rate @p_0@ supplied to 'config' -- bets
--   get clipped to @[0, lambda_max]@ so that the wealth factor
--   @1 + lambda * (x - p_0)@ stays nonnegative for both @x = 0@ and
--   @x = 1@.
--
--   * 'Fixed' always bets the supplied constant @lambda@. The wager
--     does not respond to observed data; this strategy is useful only
--     as a baseline.
--
--   * 'Adaptive' is the Bernoulli analogue of the aGRAPA bettor of
--     Waudby-Smith & Ramdas (2024). It tracks the empirical mean
--     @mu@ and variance @sigma^2@ of centred observations and bets
--     the Kelly-optimal plug-in @lambda* = mu \/ (sigma^2 + mu^2)@
--     clipped to @[0, lambda_max]@.
--
--   * 'Newton' is the online Newton step (ONS) bettor. The per-step
--     log-wealth loss @-log(1 + lambda * z)@ is convex in @lambda@;
--     ONS performs one Newton step per observation, accumulating
--     squared gradients to scale the update.
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Adaptive
  | Newton
  deriving (Eq, Show)

-- | Test outcome at the current sample count.
--
--   'Reject' means the wealth process has crossed the threshold, so
--   @H_0@ is rejected at level @alpha@. 'Continue' means there is
--   not yet enough evidence; collect more samples (or stop and
--   report no rejection -- the type-I error guarantee holds for
--   /any/ stopping rule).
data Verdict =
    Reject
  | Continue
  deriving (Eq, Show)

-- bettor state. one constructor per 'Bettor' alternative; the
-- constructor used in a given 'State' matches the 'Bettor' chosen in
-- the enclosing 'Config'.
data BetState =
    SFixed
  | SAdaptive
      {-# UNPACK #-} !Double  -- sum of z (centred observation)
      {-# UNPACK #-} !Double  -- sum of z^2 (for online variance)
      {-# UNPACK #-} !Int     -- count
  | SNewton
      {-# UNPACK #-} !Double  -- current bet lambda
      {-# UNPACK #-} !Double  -- running sum of per-step squared gradients

-- | Bernoulli rate test configuration. Build with 'config'.
--
--   Carries the bettor strategy, the baseline rate, the significance
--   level, the precomputed log-wealth rejection threshold, and the
--   safe-bet ceiling derived from @p_0@.
data Config = Config {
    -- ^ bettor strategy
    cfg_bettor     :: !Bettor
    -- ^ safe-bet ceiling
  , cfg_lam_max    :: {-# UNPACK #-} !Double
    -- ^ baseline rate @p_0@
  , cfg_p0         :: {-# UNPACK #-} !Double
    -- ^ significance level @alpha@
  , cfg_alpha      :: {-# UNPACK #-} !Double
    -- ^ rejection threshold @log(1 \/ alpha)@
  , cfg_log_thresh :: {-# UNPACK #-} !Double
  }

-- | Streaming test state. Construct with 'initial' and fold
--   observations through 'update'.
--
--   Carries the sample count, running log-wealth, and whatever
--   per-step state the chosen 'Bettor' needs.
data State = State {
    st_n     :: {-# UNPACK #-} !Int       -- ^ sample count
  , st_log_w :: {-# UNPACK #-} !Double    -- ^ running log-wealth
  , st_bet   :: !BetState                 -- ^ bettor state
  }

-- internal -------------------------------------------------------------------

-- per-bettor initial state.
init_bet :: Bettor -> BetState
init_bet b = case b of
  Fixed _  -> SFixed
  Adaptive -> SAdaptive 0 0 0
  Newton   -> SNewton 0 1.0e-6  -- small acc seed avoids div-by-zero
{-# INLINE init_bet #-}

-- compute the next bet 'lambda' from the bettor and its current
-- state. for Adaptive we form a Kelly-style plug-in from the running
-- sample mean and variance; for Newton the bet is just the last
-- lambda chosen by the Newton step (updated during 'step_bet').
bet_lambda :: Bettor -> Double -> BetState -> Double
bet_lambda b !lam_max !s = case b of
  Fixed lam -> lam
  Adaptive -> case s of
    SAdaptive !sm !sm2 !n
      | n == 0    -> 0
      | otherwise ->
          let !nd  = fromIntegral n
              !mu  = sm / nd
              !mu2 = mu * mu
              !var = max 0 (sm2 / nd - mu2)
              !den = var + mu2
              !raw = if den == 0 then 0 else mu / den
          in  max 0 (min lam_max raw)
    _ -> 0
  Newton -> case s of
    SNewton !lam _ -> lam
    _              -> 0
{-# INLINE bet_lambda #-}

-- update bettor state with newly observed centred value 'z'. for
-- Adaptive this is just accumulating sums; for Newton we take one
-- Newton step on the per-step log-wealth loss '-log(1 + lambda * z)',
-- accumulating squared gradients for adaptive scaling.
step_bet :: Bettor -> Double -> BetState -> Double -> BetState
step_bet b !lam_max !s !z = case b of
  Fixed _ -> SFixed
  Adaptive -> case s of
    SAdaptive !sm !sm2 !n -> SAdaptive (sm + z) (sm2 + z * z) (n + 1)
    _                     -> SAdaptive z (z * z) 1
  Newton -> case s of
    SNewton !lam !acc ->
      let !denom = 1 + lam * z
          !g     = if denom == 0 then 0 else negate z / denom
          !acc'  = acc + g * g
          !lam'  = lam - g / acc'
          !clp   = max 0 (min lam_max lam')
      in  SNewton clp acc'
    _ -> SNewton 0 1.0e-6
{-# INLINE step_bet #-}

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the Bernoulli rate test.
--
--   The safe-bet ceiling @lambda_max@ is set so that the wealth
--   factor @1 + lambda * (x - p_0)@ stays nonnegative for both
--   @x = 0@ and @x = 1@. The binding constraint is @x = 0@, which
--   requires @lambda <= 1 \/ p_0@; the ceiling stored is half this
--   to leave numerical margin -- the WSR safety recommendation.
--
--   @p_0@ must lie strictly in @(0, 1)@ and @alpha@ strictly in
--   @(0, 1)@. The degenerate case @p_0 = 0@ would make @lambda_max@
--   infinite (any divergence would reject immediately and the test
--   becomes uninteresting); the caller is expected to pass a small
--   positive baseline.
--
--   >>> let cfg = config 1.0e-3 0.05 Newton
config
  :: Double  -- ^ significance level @alpha@, in @(0, 1)@
  -> Double  -- ^ baseline rate @p_0@, in @(0, 1)@
  -> Bettor  -- ^ bettor strategy
  -> Config
config !alpha !p0 !b = Config {
    cfg_bettor     = b
  , cfg_lam_max    = 0.5 / p0
  , cfg_p0         = p0
  , cfg_alpha      = alpha
  , cfg_log_thresh = log (1 / alpha)
  }
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
--
--   Log-wealth starts at @0@ (i.e., wealth @1@) and the bettor
--   starts in the per-strategy initial state appropriate for the
--   'Bettor' chosen in the 'Config'.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial Config{..} = State {
    st_n     = 0
  , st_log_w = 0
  , st_bet   = init_bet cfg_bettor
  }
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one observation into the running 'State'.
--
--   @True@ means @x_t = 1@ (the event of interest occurred -- e.g.,
--   two readings diverged); @False@ means @x_t = 0@ (they matched).
--   The caller decides what \"matched\" means at the application
--   level.
--
--   Computes the centred observation @z = x - p_0@, queries the
--   bettor for its predictable bet, accumulates log-wealth via
--
--       @log_w' = log_w + log (1 + lambda * z)@
--
--   and then steps the bettor state given the newly observed @z@.
--
--   >>> let s1 = update cfg s0 True
update :: Config -> State -> Bool -> State
update Config{..} State{..} !x =
  let !xd     = if x then 1 else 0
      !z      = xd - cfg_p0
      !lam    = bet_lambda cfg_bettor cfg_lam_max st_bet
      !fac    = 1 + lam * z
      !logw'  = st_log_w + log fac
      !s'     = step_bet cfg_bettor cfg_lam_max st_bet z
  in  State (st_n + 1) logw' s'
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff log-wealth has crossed the threshold
--   @log(1 \/ alpha)@; equivalently, wealth has exceeded
--   @1 \/ alpha@. Under @H_0@, by Ville's inequality, the
--   probability of this ever happening is at most @alpha@ -- and
--   crucially this bound holds at /every/ sample size
--   simultaneously, so the user is free to peek at the verdict as
--   often as they like and stop on the first 'Reject'.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_log_w >= cfg_log_thresh = Reject
  | otherwise                  = Continue
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current log-wealth.
--
--   This is the natural \"test statistic\": it is monotone (in
--   expectation under @H_1@) in the evidence against @H_0@
--   accumulated so far, and the test rejects exactly when it crosses
--   @log(1 \/ alpha)@.
--
--   >>> log_wealth s0
--   0.0
log_wealth :: State -> Double
log_wealth = st_log_w
{-# INLINE log_wealth #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
