{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Statistics.EProcess.Mean
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Two-sided bounded-mean anytime-valid test.
--
-- For samples @x_t@ in @[lo, hi]@, tests @H_0: E[x] = m@ against
-- @H_1: E[x] /= m@. Runs two e-processes simultaneously (one per
-- direction) and combines them by Bonferroni: reject if either side's
-- wealth crosses @2 \/ alpha@.
--
-- The test is anytime-valid: type-I error is controlled at @alpha@
-- regardless of when the user stops streaming samples.

module Statistics.EProcess.Mean (
  -- * Test configuration and state
    Config
  , State
  , Verdict(..)

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
import Statistics.EProcess.Bettor

-- types ----------------------------------------------------------------------

-- | Test outcome at the current sample count.
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
      {-# UNPACK #-} !Double  -- sum of z
      {-# UNPACK #-} !Double  -- sum of z^2
      {-# UNPACK #-} !Int     -- count
  | SOns
      {-# UNPACK #-} !Double  -- lambda
      {-# UNPACK #-} !Double  -- acc (sum of squared gradients)

-- | Bounded-mean test configuration. Build with 'config'.
data Config = Config {
    cfg_bettor      :: !Bettor
  , cfg_lam_max_pos :: {-# UNPACK #-} !Double
  , cfg_lam_max_neg :: {-# UNPACK #-} !Double
  , cfg_null_mean   :: {-# UNPACK #-} !Double
  , cfg_alpha       :: {-# UNPACK #-} !Double
  , cfg_log_thresh  :: {-# UNPACK #-} !Double
  }

-- | Streaming test state. Construct with 'initial' and fold
--   observations through 'update'.
data State = State {
    st_n         :: {-# UNPACK #-} !Int
  , st_log_w_pos :: {-# UNPACK #-} !Double
  , st_log_w_neg :: {-# UNPACK #-} !Double
  , st_bet_pos   :: !BetState
  , st_bet_neg   :: !BetState
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
  Ons     -> SOns 0 1.0e-6
{-# INLINE init_bet #-}

-- compute the next bet 'lambda' from the bettor and its current
-- state; 'lam_max' is the direction-specific safety bound.
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

-- update bettor state with newly observed centred value 'z'.
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
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.5 0.0 1.0 1.0e-3 B.Ons
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
-- NB. lambda_max values are half the geometric ceiling; the 1/2 margin
--     keeps the wealth factor bounded away from zero at the boundary,
--     which is the WSR safety recommendation.
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
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
--   Bonferroni-adjusted threshold @log(2 \/ alpha)@.
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
