{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.Bernoulli.TwoSided
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Two-sided Bernoulli rate anytime-valid test. Companion to
-- "Numeric.Eproc.Bernoulli", which handles the one-sided case;
-- reach for this module when you want to test
--
--     @H_0: E[x_t | F_{t-1}] = p_0   for all t@
--
-- against the negation. The canonical case is the sign test at
-- @p_0 = 1\/2@.
--
-- The construction is the convex hedge of Waudby-Smith & Ramdas
-- (2024) §4: two per-direction Bernoulli capital processes
-- @K^+_t@ (betting against @p > p_0@ via @z = x - p_0@) and
-- @K^-_t@ (betting against @p < p_0@ via @-z@) are combined into
-- the hedged e-process @K_t = (K^+_t + K^-_t) \/ 2@ with
-- @E[K_0] = 1@. By Ville's inequality
-- @P(sup_t K_t >= 1 \/ alpha) <= alpha@, so the test rejects when
-- the supremum of @K^+_t + K^-_t@ has ever crossed @2 \/ alpha@;
-- the threshold is @log(2 \/ alpha)@. This is the same construction
-- "Numeric.Eproc.Bounded" uses to combine its two directional
-- processes.
--
-- The test is /anytime-valid/ and rejection is /latched/ in the
-- running state.
--
-- == Example
--
-- Sign test at @p_0 = 1\/2@ with a downward shift:
--
-- >>> let Right cfg = config 0.5 1.0e-3 Newton
-- >>> let s0 = initial cfg
-- >>> let xs = take 500 (cycle [False, False, False, True])
-- >>> decide cfg (foldl' (update cfg) s0 xs)
-- Reject

module Numeric.Eproc.Bernoulli.TwoSided (
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
  , samples
  ) where

import Numeric.Eproc.Common (
    Bettor(..), Verdict(..), ConfigError(..)
  , BetState, init_bet, bet_lambda, step_bet
  , finite, log_sum_exp
  )

-- types ----------------------------------------------------------------------

-- | Two-sided Bernoulli rate test configuration. Build with 'config'.
--
--   Carries the bettor strategy, the baseline rate, the significance
--   level, the precomputed convex-hedge log-wealth threshold
--   @log(2 \/ alpha)@, and the per-direction safe-bet ceilings.
data Config = Config {
    cfg_bettor      :: !Bettor
  , cfg_lam_max_pos :: {-# UNPACK #-} !Double  -- 0.5 / p0
  , cfg_lam_max_neg :: {-# UNPACK #-} !Double  -- 0.5 / (1 - p0)
  , cfg_p0          :: {-# UNPACK #-} !Double
  , cfg_alpha       :: {-# UNPACK #-} !Double
  , cfg_log_thresh  :: {-# UNPACK #-} !Double  -- log(2/alpha)
  }

-- | Streaming test state. Construct with 'initial' and fold
--   observations through 'update'.
--
--   The two log-wealth fields track the running log-wealth of the
--   positive- and negative-direction Bernoulli e-processes
--   separately; the /max log-sum/ field latches the supremum so
--   far of @log(K^+_t + K^-_t)@, which is the statistic the
--   convex-hedge construction actually monitors.
data State = State {
    st_n           :: {-# UNPACK #-} !Int
  , st_log_w_pos   :: {-# UNPACK #-} !Double
  , st_log_w_neg   :: {-# UNPACK #-} !Double
  , st_max_log_sum :: {-# UNPACK #-} !Double
  , st_bet_pos     :: !BetState
  , st_bet_neg     :: !BetState
  }

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the two-sided Bernoulli rate test.
--
--   Per-direction safe-bet ceilings are @0.5 \/ p_0@ (positive) and
--   @0.5 \/ (1 - p_0)@ (negative), chosen so that each wealth factor
--   stays nonnegative for both admissible observations. The
--   threshold is @log(2 \/ alpha)@; the 2 reflects that the
--   convex-hedge test monitors the sum @K^+ + K^-@, whose initial
--   value is @2@ (each side starts at @K = 1@).
--
--   Returns 'Left' with a 'ConfigError' on inputs that would leave
--   the mathematical regime: either of @p_0@ or @alpha@ non-finite
--   (NaN or infinite); @p_0@ outside @(0, 1)@; or @alpha@ outside
--   @(0, 1)@.
--
--   >>> let Right cfg = config 0.5 1.0e-3 Newton
config
  :: Double  -- ^ baseline rate @p_0@, in @(0, 1)@
  -> Double  -- ^ significance level @alpha@, in @(0, 1)@
  -> Bettor  -- ^ bettor strategy
  -> Either ConfigError Config
config !p0 !alpha !b
  | not (finite p0 && p0 > 0 && p0 < 1) =
      Left (InvalidBaselineRate p0)
  | not (finite alpha && alpha > 0 && alpha < 1) =
      Left (InvalidAlpha alpha)
  | otherwise = Right Config {
        cfg_bettor      = b
      , cfg_lam_max_pos = 0.5 / p0
      , cfg_lam_max_neg = 0.5 / (1 - p0)
      , cfg_p0          = p0
      , cfg_alpha       = alpha
      , cfg_log_thresh  = log (2 / alpha)
      }
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
--
--   Both per-direction log-wealths start at @0@ (i.e., @K = 1@);
--   the max-log-sum starts at @log 2@ (since @K^+_0 + K^-_0 = 2@);
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
      , st_max_log_sum = log 2
      , st_bet_pos     = s0
      , st_bet_neg     = s0
      }
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one observation into the running 'State'.
--
--   Computes the centred observation @z = x - p_0@, queries the two
--   directional bettors, accumulates per-direction log-wealth, then
--   updates the running supremum of @log(K^+ + K^-)@ via
--   log-sum-exp and steps the bettor states.
--
--   >>> let s1 = update cfg s0 True
update :: Config -> State -> Bool -> State
update Config{..} State{..} !x =
  let !xd      = if x then 1 else 0
      !z       = xd - cfg_p0
      !lam_p   = bet_lambda cfg_bettor cfg_lam_max_pos st_bet_pos
      !lam_n   = bet_lambda cfg_bettor cfg_lam_max_neg st_bet_neg
      !fac_p   = 1 + lam_p * z
      !fac_n   = 1 - lam_n * z
      !logw_p  = st_log_w_pos + log fac_p
      !logw_n  = st_log_w_neg + log fac_n
      !log_sum = log_sum_exp logw_p logw_n
      !max_sum = max st_max_log_sum log_sum
      !sp      = step_bet cfg_bettor cfg_lam_max_pos st_bet_pos z
      !sn      = step_bet cfg_bettor cfg_lam_max_neg st_bet_neg (negate z)
  in  State (st_n + 1) logw_p logw_n max_sum sp sn
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff the supremum-so-far of @log(K^+_t + K^-_t)@ has
--   crossed @log(2 \/ alpha)@ at some point; equivalently the
--   convex-hedge e-process @(K^+ + K^-) \/ 2@ has exceeded
--   @1 \/ alpha@. Under @H_0@, Ville's inequality bounds the
--   probability of this ever happening by @alpha@, uniformly
--   across sample counts.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_max_log_sum >= cfg_log_thresh = Reject
  | otherwise                        = Continue
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The supremum-so-far of @log(K^+_t + K^-_t)@. Monotone
--   nondecreasing; starts at @log 2@ (since @K^+_0 + K^-_0 = 2@).
--
--   >>> log_wealth s0
--   0.6931471805599453
log_wealth :: State -> Double
log_wealth = st_max_log_sum
{-# INLINE log_wealth #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
