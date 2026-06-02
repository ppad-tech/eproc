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
-- direction) and combines them by Bonferroni: reject if either
-- side's wealth crosses @2 \/ alpha@.
--
-- The test is anytime-valid: type-I error is controlled at @alpha@
-- regardless of when the user stops streaming samples.

module Statistics.EProcess.Mean (
    -- * Types
    Config
  , State
  , Verdict(..)

    -- * Construction
  , config

    -- * Streaming interface
  , initial
  , update
  , decide

    -- * Inspection
  , logWealth
  , samples
  ) where

import GHC.Exts (Double(D#))
import Statistics.EProcess.Bettor

-- | Test outcome at the current sample count.
data Verdict = Reject | Continue
  deriving (Eq, Show)

-- per-direction bettor state. one constructor per 'Bettor'
-- alternative; the constructor used in a given 'State' matches the
-- 'Bettor' chosen in the surrounding 'Config'.
data BetState
  = SFixed
  | SAgrapa
      {-# UNPACK #-} !Double  -- sum of z
      {-# UNPACK #-} !Double  -- sum of z^2
      {-# UNPACK #-} !Int     -- count
  | SOns
      {-# UNPACK #-} !Double  -- lambda
      {-# UNPACK #-} !Double  -- acc (sum of squared gradients)

-- | Test configuration. Constructed by 'config'.
data Config = Config
  { cfgBettor    :: !Bettor
  , cfgLamMaxPos :: {-# UNPACK #-} !Double
  , cfgLamMaxNeg :: {-# UNPACK #-} !Double
  , cfgNullMean  :: {-# UNPACK #-} !Double
  , cfgAlpha     :: {-# UNPACK #-} !Double
  , cfgLogThresh :: {-# UNPACK #-} !Double
  }

-- | Test state. Two log-wealth processes (one per direction) and
--   per-direction bettor state.
data State = State
  { stN       :: {-# UNPACK #-} !Int
  , stLogWPos :: {-# UNPACK #-} !Double
  , stLogWNeg :: {-# UNPACK #-} !Double
  , stBetPos  :: !BetState
  , stBetNeg  :: !BetState
  }

-- floor for the wealth factor before taking a log; keeps the running
-- log-wealth finite when a step pushes the factor to (or below) zero.
-- NB. written via MagicHash because the fractional literal '1.0e-300'
--     compiles as 'fromRational (1.0e-300 :: Rational)', and GHC does
--     not constant-fold the conversion -- leaving a per-step
--     '$wrationalToDouble' call in the worker.
tiny :: Double
tiny = D# 1.0e-300##
{-# INLINE tiny #-}

-- | Build a test configuration.
--
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.5 0.0 1.0 1.0e-6 B.Ons
config
  :: Double  -- ^ null mean @m@
  -> Double  -- ^ sample lower bound @lo@
  -> Double  -- ^ sample upper bound @hi@
  -> Double  -- ^ significance level @alpha@
  -> Bettor  -- ^ bettor strategy
  -> Config
config !m !lo !hi !alpha !b = Config
  { cfgBettor    = b
  , cfgLamMaxPos = 0.5 / (m - lo)
  , cfgLamMaxNeg = 0.5 / (hi - m)
  , cfgNullMean  = m
  , cfgAlpha     = alpha
  , cfgLogThresh = log (2 / alpha)
  }
-- NB. the lambda_max values are half the geometric ceiling; the 1/2
--    margin keeps the wealth factor bounded away from zero at the
--    boundary, which is the WSR safety recommendation.
{-# INLINE config #-}

-- per-bettor initial state.
initBet :: Bettor -> BetState
initBet b = case b of
  Fixed _ -> SFixed
  Agrapa  -> SAgrapa 0 0 0
  Ons     -> SOns 0 1.0e-6
{-# INLINE initBet #-}

-- | Initial state for streaming.
initial :: Config -> State
initial Config{..} =
  let !s0 = initBet cfgBettor
  in  State
        { stN       = 0
        , stLogWPos = 0
        , stLogWNeg = 0
        , stBetPos  = s0
        , stBetNeg  = s0
        }
{-# INLINE initial #-}

-- compute the next bet @lambda@ from the bettor and its current
-- state. @lamMax@ is the direction-specific safety bound.
betLambda :: Bettor -> Double -> BetState -> Double
betLambda b !lamMax !s = case b of
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
          in  max 0 (min lamMax raw)
    _ -> 0
  Ons -> case s of
    SOns !lam _ -> lam
    _           -> 0
{-# INLINE betLambda #-}

-- update bettor state with newly observed centred value @z@.
stepBet :: Bettor -> Double -> BetState -> Double -> BetState
stepBet b !lamMax !s !z = case b of
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
          !clp   = max 0 (min lamMax lam')
      in  SOns clp acc'
    _ -> SOns 0 1.0e-6
{-# INLINE stepBet #-}

-- | Fold one observation into the state.
update :: Config -> State -> Double -> State
update Config{..} State{..} !x =
  let !z      = x - cfgNullMean
      !lamP   = betLambda cfgBettor cfgLamMaxPos stBetPos
      !lamN   = betLambda cfgBettor cfgLamMaxNeg stBetNeg
      !facP   = 1 + lamP * z
      !facN   = 1 - lamN * z
      !logWP' = stLogWPos + log (max tiny facP)
      !logWN' = stLogWNeg + log (max tiny facN)
      !sP'    = stepBet cfgBettor cfgLamMaxPos stBetPos z
      !sN'    = stepBet cfgBettor cfgLamMaxNeg stBetNeg (negate z)
  in  State (stN + 1) logWP' logWN' sP' sN'
{-# INLINE update #-}

-- | Decide based on current wealth.
--
--   'Reject' iff either directional log-wealth has crossed the
--   Bonferroni-adjusted threshold @log(2 \/ alpha)@.
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | stLogWPos >= cfgLogThresh = Reject
  | stLogWNeg >= cfgLogThresh = Reject
  | otherwise                 = Continue
{-# INLINE decide #-}

-- | Current log-wealth (the larger of the two directional processes).
logWealth :: State -> Double
logWealth State{..} = max stLogWPos stLogWNeg
{-# INLINE logWealth #-}

-- | Sample count consumed so far.
samples :: State -> Int
samples = stN
{-# INLINE samples #-}
