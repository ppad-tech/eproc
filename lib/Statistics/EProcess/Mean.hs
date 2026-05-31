{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
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

import Statistics.EProcess.Bettor

-- | Test outcome at the current sample count.
data Verdict = Reject | Continue
  deriving (Eq, Show)

-- | Test configuration. Constructed by 'config'.
data Config s = Config
  { cfgBetPos    :: !(Bettor s)
  , cfgBetNeg    :: !(Bettor s)
  , cfgNullMean  :: {-# UNPACK #-} !Double
  , cfgAlpha     :: {-# UNPACK #-} !Double
  , cfgLogThresh :: {-# UNPACK #-} !Double
  }

-- | Test state. Two log-wealth processes (one per direction) and
--   per-direction bettor state.
data State s = State
  { stN       :: {-# UNPACK #-} !Int
  , stLogWPos :: {-# UNPACK #-} !Double
  , stLogWNeg :: {-# UNPACK #-} !Double
  , stBetPos  :: !s
  , stBetNeg  :: !s
  }

-- | Build a test configuration.
--
--   The bettor argument is a function from @lambda_max@ to a bettor;
--   the same builder is used for both directions, with appropriate
--   bounds computed from @lo@, @hi@, and @m@.
--
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.5 0.0 1.0 1.0e-6 B.ons
config
  :: Double                -- ^ null mean @m@
  -> Double                -- ^ sample lower bound @lo@
  -> Double                -- ^ sample upper bound @hi@
  -> Double                -- ^ significance level @alpha@
  -> (Double -> Bettor s)  -- ^ bettor builder, taking @lambda_max@
  -> Config s
config !m !lo !hi !alpha mk = Config
  { cfgBetPos    = mk (0.5 / (m - lo))
  , cfgBetNeg    = mk (0.5 / (hi - m))
  , cfgNullMean  = m
  , cfgAlpha     = alpha
  , cfgLogThresh = log (2 / alpha)
  }
-- NB. argument to @mk@ is half the geometric @lambda_max@; the 1/2
--    margin keeps the wealth factor bounded away from zero at the
--    boundary, which is the WSR safety recommendation.

-- | Initial state for streaming.
initial :: Config s -> State s
initial Config{..} = State
  { stN       = 0
  , stLogWPos = 0
  , stLogWNeg = 0
  , stBetPos  = bettorInit cfgBetPos
  , stBetNeg  = bettorInit cfgBetNeg
  }

-- | Fold one observation into the state.
update :: Config s -> State s -> Double -> State s
update Config{..} State{..} !x =
  let !z      = x - cfgNullMean
      !lamP   = bettorBet cfgBetPos stBetPos
      !lamN   = bettorBet cfgBetNeg stBetNeg
      !facP   = 1 + lamP * z
      !facN   = 1 - lamN * z
      !logWP' = stLogWPos + log (max 1.0e-300 facP)
      !logWN' = stLogWNeg + log (max 1.0e-300 facN)
      !sP'    = bettorStep cfgBetPos stBetPos z
      !sN'    = bettorStep cfgBetNeg stBetNeg (negate z)
  in  State (stN + 1) logWP' logWN' sP' sN'

-- | Decide based on current wealth.
--
--   'Reject' iff either directional log-wealth has crossed the
--   Bonferroni-adjusted threshold @log(2 \/ alpha)@.
decide :: Config s -> State s -> Verdict
decide Config{..} State{..}
  | stLogWPos >= cfgLogThresh = Reject
  | stLogWNeg >= cfgLogThresh = Reject
  | otherwise                 = Continue

-- | Current log-wealth (the larger of the two directional processes).
logWealth :: State s -> Double
logWealth State{..} = max stLogWPos stLogWNeg

-- | Sample count consumed so far.
samples :: State s -> Int
samples = stN
