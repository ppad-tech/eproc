{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.Bounded
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Two-sided bounded-mean anytime-valid test.
--
-- For samples @x_t@ in @[lo, hi]@, tests @H_0: E[x] = m@ against
-- @H_1: E[x] /= m@.
--
-- Internally two one-sided e-processes are run in parallel: a
-- /positive-direction/ process betting against the alternative
-- @E[x] > m@ (using centred observations @z = x - m@), and a
-- /negative-direction/ process betting against @E[x] < m@ (using
-- @-z@). Each maintains its own log-wealth and bettor state. The
-- test rejects when either side's wealth crosses @2 \/ alpha@; the
-- factor of 2 is the Bonferroni adjustment for the two-sided union.
--
-- The test is /anytime-valid/: under @H_0@ the wealth process is a
-- nonnegative supermartingale, so by Ville's inequality the
-- probability of ever crossing the threshold is at most @alpha@,
-- regardless of when the user decides to stop streaming samples.

module Numeric.Eproc.Bounded (
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

import GHC.Exts (Double(D#))

-- types ----------------------------------------------------------------------

-- | A predictable bettor.
--
--   A bettor describes how, given the history of centred observations
--   @z_t = x_t - m@, the next predictable bet @lambda_t@ is chosen.
--   Predictability -- that is, @lambda_t@ depends only on data
--   observed strictly before step @t@ -- is what makes the resulting
--   wealth process a nonnegative supermartingale under @H_0@.
--
--   For 'Agrapa' and 'Ons', a per-direction safe-bet ceiling
--   @lambda_max@ is derived from the sample bounds supplied to
--   'config' -- bets get clipped to @[0, lambda_max]@ so that the
--   wealth factor @1 + lambda * z@ stays nonnegative for every
--   admissible observation.
--
--   * 'Fixed' always bets the supplied constant @lambda@. The wager
--     does not respond to observed data; this strategy is useful only
--     as a baseline.
--
--   * 'Agrapa' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor of Waudby-Smith & Ramdas (2024).
--     It tracks the empirical mean @mu@ and variance @sigma^2@ of
--     centred observations and bets the Kelly-optimal plug-in
--     @lambda* = mu \/ (sigma^2 + mu^2)@ clipped to
--     @[0, lambda_max]@. Fast to compute and competitive in practice.
--
--   * 'Ons' is the online Newton step bettor. The per-step log-wealth
--     loss @-log(1 + lambda * z)@ is convex in @lambda@; ONS performs
--     one Newton step per observation, accumulating squared gradients
--     to scale the update. Achieves logarithmic regret against the
--     best constant bet in hindsight and is in practice the strongest
--     of the three bettors under most signal regimes.
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Agrapa
  | Ons
  deriving (Eq, Show)

-- | Test outcome at the current sample count.
--
--   'Reject' means the wealth process has crossed the Bonferroni
--   threshold, so @H_0@ is rejected at level @alpha@. 'Continue'
--   means there is not yet enough evidence; collect more samples (or
--   stop and report no rejection -- the type-I error guarantee holds
--   for /any/ stopping rule).
data Verdict =
    Reject
  | Continue
  deriving (Eq, Show)

-- per-direction bettor state. one constructor per 'Bettor' alternative;
-- the constructor used in a given 'State' matches the 'Bettor' chosen
-- in the enclosing 'Config'.
data BetState =
    SFixed
  | SAgrapa
      {-# UNPACK #-} !Double  -- sum of z (centred observation)
      {-# UNPACK #-} !Double  -- sum of z^2 (for online variance)
      {-# UNPACK #-} !Int     -- count
  | SOns
      {-# UNPACK #-} !Double  -- current bet lambda
      {-# UNPACK #-} !Double  -- running sum of per-step squared gradients

-- | Bounded-mean test configuration. Build with 'config'.
--
--   Carries the bettor strategy, the null mean, the significance
--   level, the precomputed Bonferroni-adjusted log-wealth threshold,
--   and the per-direction safe-bet ceilings (see 'config' for how
--   the latter are derived from the sample bounds).
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
--   positive- and negative-direction e-processes separately;
--   'decide' compares each to the threshold and 'log_wealth' returns
--   the larger of the two. The per-direction bettor states carry
--   whatever the chosen 'Bettor' needs (running sums, current bet,
--   etc.).
data State = State {
    st_n         :: {-# UNPACK #-} !Int       -- ^ sample count
  , st_log_w_pos :: {-# UNPACK #-} !Double    -- ^ log-wealth, pos-dir process
  , st_log_w_neg :: {-# UNPACK #-} !Double    -- ^ log-wealth, neg-dir process
  , st_bet_pos   :: !BetState                 -- ^ bettor state, pos-direction
  , st_bet_neg   :: !BetState                 -- ^ bettor state, neg-direction
  }

-- internal -------------------------------------------------------------------

-- floor for the wealth factor before taking a log; keeps the running
-- log-wealth finite when a step pushes the factor to (or below) zero.
-- NB. written via MagicHash because the fractional literal '1.0e-300'
--     compiles as 'fromRational (1.0e-300 :: Rational)', and GHC does
--     not constant-fold the conversion -- leaving a per-step
--     '$wrationalToDouble' call in the worker.
tiny :: Double
tiny = D# 1.0e-300##
{-# INLINE tiny #-}

-- per-bettor initial state.
init_bet :: Bettor -> BetState
init_bet b = case b of
  Fixed _ -> SFixed
  Agrapa  -> SAgrapa 0 0 0
  Ons     -> SOns 0 1.0e-6  -- small acc seed avoids div-by-zero on first step
{-# INLINE init_bet #-}

-- compute the next bet 'lambda' from the bettor and its current
-- state; 'lam_max' is the direction-specific safety bound. for
-- Agrapa we form a Kelly-style plug-in from the running sample mean
-- and variance; for Ons the bet is just the last lambda chosen by the
-- Newton step (updated during 'step_bet').
bet_lambda :: Bettor -> Double -> BetState -> Double
bet_lambda b !lam_max !s = case b of
  Fixed lam -> lam
  Agrapa -> case s of
    SAgrapa !sm !sm2 !n
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
  Ons -> case s of
    SOns !lam _ -> lam
    _           -> 0
{-# INLINE bet_lambda #-}

-- update bettor state with newly observed centred value 'z'. for
-- Agrapa this is just accumulating sums; for Ons we take one Newton
-- step on the per-step log-wealth loss '-log(1 + lambda * z)',
-- accumulating squared gradients for adaptive scaling.
step_bet :: Bettor -> Double -> BetState -> Double -> BetState
step_bet b !lam_max !s !z = case b of
  Fixed _ -> SFixed
  Agrapa -> case s of
    SAgrapa !sm !sm2 !n -> SAgrapa (sm + z) (sm2 + z * z) (n + 1)
    _                   -> SAgrapa z (z * z) 1
  Ons -> case s of
    SOns !lam !acc ->
      let !denom = 1 + lam * z
          !g     = if denom == 0 then 0 else negate z / denom
          !acc'  = acc + g * g
          !lam'  = lam - g / acc'
          !clp   = max 0 (min lam_max lam')
      in  SOns clp acc'
    _ -> SOns 0 1.0e-6
{-# INLINE step_bet #-}

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
--   @log(2 \/ alpha)@; the 2 is the Bonferroni union-bound
--   adjustment for the two one-sided e-processes.
--
--   >>> let cfg = config 0.5 0.0 1.0 1.0e-3 Ons
config
  :: Double  -- ^ null mean @m@
  -> Double  -- ^ sample lower bound @lo@
  -> Double  -- ^ sample upper bound @hi@
  -> Double  -- ^ significance level @alpha@
  -> Bettor  -- ^ bettor strategy
  -> Config
config !m !lo !hi !alpha !b = Config {
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
--   Both directional log-wealths start at @0@ (i.e., wealth @1@) and
--   both bettors start in the per-strategy initial state appropriate
--   for the 'Bettor' chosen in the 'Config'.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial Config{..} =
  let !s0 = init_bet cfg_bettor
  in  State {
        st_n         = 0
      , st_log_w_pos = 0
      , st_log_w_neg = 0
      , st_bet_pos   = s0
      , st_bet_neg   = s0
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
--   (with the symmetric @-lambda@ for the negative direction), and
--   then steps the bettor states given the newly observed @z@. The
--   per-step wealth factor is floored at a tiny positive value to
--   keep the log finite when a marginal bet drives the factor to (or
--   below) zero.
--
--   >>> let s1 = update cfg s0 0.7
update :: Config -> State -> Double -> State
update Config{..} State{..} !x =
  let !z      = x - cfg_null_mean
      !lam_p  = bet_lambda cfg_bettor cfg_lam_max_pos st_bet_pos
      !lam_n  = bet_lambda cfg_bettor cfg_lam_max_neg st_bet_neg
      !fac_p  = 1 + lam_p * z
      !fac_n  = 1 - lam_n * z
      !logw_p = st_log_w_pos + log (max tiny fac_p)
      !logw_n = st_log_w_neg + log (max tiny fac_n)
      !sp     = step_bet cfg_bettor cfg_lam_max_pos st_bet_pos z
      !sn     = step_bet cfg_bettor cfg_lam_max_neg st_bet_neg (negate z)
  in  State (st_n + 1) logw_p logw_n sp sn
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff either directional log-wealth has crossed the
--   Bonferroni-adjusted threshold @log(2 \/ alpha)@; equivalently,
--   the wealth process on either side has exceeded @2 \/ alpha@.
--   Under @H_0@, by Ville's inequality, the probability of this ever
--   happening is at most @alpha@ -- and crucially this bound holds
--   at /every/ sample size simultaneously, so the user is free to
--   peek at the verdict as often as they like and stop on the first
--   'Reject'.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_log_w_pos >= cfg_log_thresh = Reject
  | st_log_w_neg >= cfg_log_thresh = Reject
  | otherwise                      = Continue
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current log-wealth, taken as the maximum of the two
--   directional processes.
--
--   This is the natural \"test statistic\": it is monotone in the
--   evidence against @H_0@ accumulated so far, and the test rejects
--   exactly when it crosses @log(2 \/ alpha)@.
--
--   >>> log_wealth s0
--   0.0
log_wealth :: State -> Double
log_wealth State{..} = max st_log_w_pos st_log_w_neg
{-# INLINE log_wealth #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
