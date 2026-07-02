{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.Bounded
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Two-sided bounded-mean anytime-valid test.
--
-- For samples @x_t@ in @[lo, hi]@, tests
--
--     @H_0: E[x_t | F_{t-1}] = m   for all t@
--
-- against the negation. Here @F_{t-1}@ is the filtration generated
-- by everything observed strictly before time @t@; the conditional
-- form is what anytime validity actually requires. For i.i.d.
-- samples this reduces to the usual marginal statement
-- @E[x] = m@; for adaptively-collected or otherwise non-i.i.d.
-- streams the conditional statement is the right thing to think
-- about.
--
-- Internally two one-sided e-processes are run in parallel: a
-- /positive-direction/ process @K^+_t@ betting against the
-- alternative @E[x_t | F_{t-1}] > m@ (using centred observations
-- @z = x - m@), and a /negative-direction/ process @K^-_t@ betting
-- against @E[x_t | F_{t-1}] < m@ (using @-z@). Each maintains its
-- own log-wealth and bettor state.
--
-- The two sides are combined via the /hedged capital process/ of
-- Waudby-Smith & Ramdas (2024) §4: their average
-- @K_t = (K^+_t + K^-_t) \/ 2@ is itself an e-process (convex
-- combinations preserve the supermartingale property), with
-- @E[K_0] = 1@. By Ville's inequality
-- @P(sup_t K_t >= 1 \/ alpha) <= alpha@, so the test rejects when
-- the supremum of @K^+_t + K^-_t@ has ever crossed @2 \/ alpha@.
--
-- This is strictly more powerful than the naive Bonferroni union
-- (reject when @max(K^+_t, K^-_t) >= 2 \/ alpha@): the convex-hedge
-- rejection region contains Bonferroni's (since
-- @K^+ + K^- >= max(K^+, K^-)@), with the same alpha guarantee.
-- For one-sided alternatives the gap is small (the losing-direction
-- bettor stays near @1@); for genuinely two-sided alternatives it
-- can be substantial.
--
-- The test is /anytime-valid/: under @H_0@ the wealth process is a
-- nonnegative supermartingale, so by Ville's inequality the
-- probability of /ever/ crossing the threshold is at most @alpha@,
-- regardless of when the user decides to stop streaming samples.
-- Rejection is /latched/ in the running state -- once a side has
-- crossed threshold, 'decide' continues to return 'Reject' even if
-- the current log-wealth has since dropped back below threshold.
--
-- == Example
--
-- Test @H_0: E[x] = 0.5@ for @x@ in @[0, 1]@ at level @alpha = 1e-3@
-- against a stream with empirical mean @0.8@:
--
-- >>> let Right cfg = config 0.5 0.0 1.0 1.0e-3 Newton
-- >>> let xs  = concat (replicate 30 [1, 1, 0, 1, 1, 0, 1, 1, 1, 1])
-- >>> decide cfg (foldl' (update cfg) (initial cfg) xs)
-- Reject

module Numeric.Eproc.Bounded (
  -- * Test configuration and state
    Config
  , State
  , Verdict(..)
  , ConfigError(..)

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
  , log_wealth_sup
  , samples
  ) where

import GHC.Float (log1p)
import Numeric.Eproc.Common (
    Bettor(..), Verdict(..), ConfigError(..)
  , BetState, init_bet, bet_lambda, step_bet
  , finite, log_sum_exp, log2_dbl
  )

-- types ----------------------------------------------------------------------

-- here, the centred observation @z_t@ referenced in
-- "Numeric.Eproc.Common" is @x_t - m@; the per-direction safe-bet
-- ceilings @lambda_max@ are derived from the sample bounds (see
-- 'config').

-- | Bounded-mean test configuration. Build with 'config'.
--
--   Carries the bettor strategy, the null mean, the significance
--   level, the precomputed convex-hedge log-wealth threshold, and
--   the per-direction safe-bet ceilings (see 'config' for how the
--   latter are derived from the sample bounds).
data Config = Config {
    -- ^ bettor strategy
    cfg_bettor      :: !Bettor
    -- ^ positive-direction safe-bet ceiling
  , cfg_lam_max_pos :: {-# UNPACK #-} !Double
    -- ^ negative-direction safe-bet ceiling
  , cfg_lam_max_neg :: {-# UNPACK #-} !Double
    -- ^ null mean @m@
  , cfg_null_mean   :: {-# UNPACK #-} !Double
    -- ^ significance level @alpha@
  , cfg_alpha       :: {-# UNPACK #-} !Double
    -- ^ rejection threshold @log(2 \/ alpha)@
  , cfg_log_thresh  :: {-# UNPACK #-} !Double
  }

-- | Streaming test state. Construct with 'initial' and fold
--   observations through 'update'.
--
--   The two log-wealth fields track the running log-wealth of the
--   positive- and negative-direction e-processes separately; the
--   /sup log-sum/ field latches the supremum so far of
--   @log(K^+_t + K^-_t)@, which is the test statistic the
--   convex-hedge construction actually monitors. The per-direction
--   bettor states carry whatever the chosen 'Bettor' needs (running
--   sums, current bet, etc.).
data State = State {
    st_n           :: {-# UNPACK #-} !Int     -- ^ sample count
  , st_log_w_pos   :: {-# UNPACK #-} !Double  -- ^ log-wealth, pos
  , st_log_w_neg   :: {-# UNPACK #-} !Double  -- ^ log-wealth, neg
  , st_sup_log_sum :: {-# UNPACK #-} !Double  -- ^ sup log(K^+ + K^-)
  , st_bet_pos     :: !BetState               -- ^ bettor state, pos
  , st_bet_neg     :: !BetState               -- ^ bettor state, neg
  }

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the bounded-mean test.
--
--   Each per-direction safe-bet ceiling @lambda_max@ is set so that
--   the wealth factor stays nonnegative for every admissible
--   observation:
--
--   * The positive-direction factor is @1 + lambda_p * (x - m)@.
--     Since @x@ can dip to @lo@, @x - m@ can reach @lo - m@ (the
--     most negative value), so we need
--     @lambda_p <= 1 \/ (m - lo)@. The ceiling stored is half this
--     to leave numerical margin -- the WSR safety recommendation.
--
--   * The negative-direction factor is @1 - lambda_n * (x - m)@.
--     Since @x@ can rise to @hi@, @x - m@ can reach @hi - m@, so we
--     need @lambda_n <= 1 \/ (hi - m)@; again the ceiling is set to
--     half this.
--
--   The log-wealth rejection threshold is precomputed as
--   @log(2 \/ alpha)@; the 2 reflects that the convex-hedge test
--   monitors the sum @K^+_t + K^-_t@, whose initial value is @2@
--   (each side starts at @K = 1@).
--
--   Returns 'Left' with a 'ConfigError' on inputs that would leave
--   the mathematical regime: any of @m@, @lo@, @hi@, @alpha@
--   non-finite (NaN or infinite); @alpha@ outside @(0, 1)@;
--   @lo >= hi@; or @m@ outside the open interval @(lo, hi)@
--   (strict, to avoid the safe-bet ceilings dividing by zero).
--
--   >>> let Right cfg = config 0.5 0.0 1.0 1.0e-3 Newton
config
  :: Double  -- ^ null mean @m@
  -> Double  -- ^ sample lower bound @lo@
  -> Double  -- ^ sample upper bound @hi@
  -> Double  -- ^ significance level @alpha@
  -> Bettor  -- ^ bettor strategy
  -> Either ConfigError Config
config !m !lo !hi !alpha !b
  | not (finite alpha && alpha > 0 && alpha < 1) =
      Left (InvalidAlpha alpha)
  | not (finite lo && finite hi && lo < hi) =
      Left (InvalidBounds lo hi)
  | not (finite m && lo < m && m < hi) =
      Left (InvalidNullMean m lo hi)
  | otherwise = Right Config {
        cfg_bettor      = b
      , cfg_lam_max_pos = 0.5 / (m - lo)
      , cfg_lam_max_neg = 0.5 / (hi - m)
      , cfg_null_mean   = m
      , cfg_alpha       = alpha
      , cfg_log_thresh  = log (2 / alpha)
      }
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
--
--   Both per-direction log-wealths start at @0@ (i.e., @K = 1@);
--   the sup-log-sum starts at @log 2@ (since @K^+_0 + K^-_0 = 2@);
--   both bettors start in the per-strategy initial state
--   appropriate for the 'Bettor' chosen in the 'Config'.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial Config{..} =
  let !s0 = init_bet cfg_bettor
  in  State {
        st_n           = 0
      , st_log_w_pos   = 0
      , st_log_w_neg   = 0
      , st_sup_log_sum = log2_dbl
      , st_bet_pos     = s0
      , st_bet_neg     = s0
      }
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one observation into the running 'State'.
--
--   Computes the centred observation @z = x - m@, queries the two
--   directional bettors for their predictable bets, accumulates
--   per-direction log-wealth via
--
--       @log_w' = log_w + log (1 + lambda * z)@
--
--   (with the symmetric @-lambda@ for the negative direction), then
--   updates the running supremum of @log(K^+ + K^-)@ via
--   log-sum-exp and steps the bettor states given the newly
--   observed @z@.
--
--   /Precondition/: @x@ must lie in the @[lo, hi]@ interval given
--   to 'config'. The type-I error guarantee of the test depends on
--   this. Out-of-range observations can drive the wealth factor
--   negative, taking the construction out of the supermartingale
--   regime entirely; the function does not check for this.
--
--   >>> let s1 = update cfg s0 0.7
update :: Config -> State -> Double -> State
update Config{..} State{..} !x =
  let !z       = x - cfg_null_mean
      !lam_p   = bet_lambda cfg_bettor cfg_lam_max_pos st_bet_pos
      !lam_n   = bet_lambda cfg_bettor cfg_lam_max_neg st_bet_neg
      !logw_p  = st_log_w_pos + log1p (lam_p * z)
      !logw_n  = st_log_w_neg + log1p (negate lam_n * z)
      -- Skip 'log_sum_exp' when the cheap upper bound
      --   log_sum_exp a b <= max a b + log 2
      -- already sits at or below the running max: no update can
      -- move it. Under H_0 (calibration) this is the common case.
      !cheap_ub = max logw_p logw_n + log2_dbl
      !sup_sum
        | cheap_ub <= st_sup_log_sum = st_sup_log_sum
        | otherwise                  =
            max st_sup_log_sum (log_sum_exp logw_p logw_n)
      !sp      = step_bet cfg_bettor cfg_lam_max_pos st_bet_pos z
      !sn      = step_bet cfg_bettor cfg_lam_max_neg st_bet_neg (negate z)
  in  State (st_n + 1) logw_p logw_n sup_sum sp sn
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff the supremum-so-far of @log(K^+_t + K^-_t)@ has
--   ever crossed the threshold @log(2 \/ alpha)@ — equivalently,
--   the convex-hedge e-process @(K^+ + K^-) \/ 2@ has exceeded
--   @1 \/ alpha@ at some point in the stream so far. Under @H_0@,
--   by Ville's inequality, the probability of this ever happening
--   is at most @alpha@ -- and crucially this bound holds at /every/
--   sample size simultaneously, so the user is free to peek at the
--   verdict as often as they like and stop on the first 'Reject'.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_sup_log_sum >= cfg_log_thresh = Reject
  | otherwise                        = Continue
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current @log(K^+_t + K^-_t)@ -- the running log-wealth of
--   the convex-hedge combination at the present sample count.
--
--   Unlike 'log_wealth_sup' this is not monotone: adverse
--   observations decrease it. It is bounded above by
--   'log_wealth_sup', which is what 'decide' tests against the
--   rejection threshold.
--
--   Starts at @log 2@ (since @K^+_0 + K^-_0 = 2@).
--
--   >>> log_wealth s0
--   0.6931471805599453
log_wealth :: State -> Double
log_wealth State{..} = log_sum_exp st_log_w_pos st_log_w_neg
{-# INLINE log_wealth #-}

-- | The supremum-so-far of @log(K^+_t + K^-_t)@, taken across all
--   sample counts up to the current one. This is the test statistic
--   the convex-hedge construction actually monitors: it is monotone
--   nondecreasing in the sample count, and 'decide' rejects exactly
--   when it crosses @log(2 \/ alpha)@.
--
--   Starts at @log 2@ (since @K^+_0 + K^-_0 = 2@).
--
--   >>> log_wealth_sup s0
--   0.6931471805599453
log_wealth_sup :: State -> Double
log_wealth_sup State{..} = st_sup_log_sum
{-# INLINE log_wealth_sup #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
