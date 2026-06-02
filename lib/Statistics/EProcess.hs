{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Statistics.EProcess
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Anytime-valid sequential hypothesis testing for bounded random
-- variables, via the e-process / betting framework of Waudby-Smith
-- and Ramdas (2024).
--
-- A bettor places predictable wagers against the null; the wealth
-- process is a nonnegative supermartingale under @H_0@, and Ville's
-- inequality gives type-I error control at @alpha@ for the stopping
-- rule \"reject the first time wealth exceeds @1\/alpha@\" —
-- regardless of when the user stops streaming samples.
--
-- This module re-exports the primary API. For finer control, see:
--
--   * "Statistics.EProcess.Bettor" for bettor strategies.
--
--   * "Statistics.EProcess.Mean" for the one-sample bounded-mean
--     test.
--
--   * "Statistics.EProcess.TwoSample" for the paired two-sample
--     mean-equality test.

module Statistics.EProcess (
    -- * Bettors
    Bettor(..)

    -- * Bounded-mean test
    --
    -- $mean
  , Mean.Verdict(..)
  , meanConfig
  , initMeanState
  , updateMean
  , decideMean

    -- * Paired two-sample test
    --
    -- $twosample
  , twoSampleConfig
  , initTwoSampleState
  , updateTwoSample
  , decideTwoSample

    -- * Inspection
  , logWealth
  , samples
  ) where

import Statistics.EProcess.Bettor
import qualified Statistics.EProcess.Mean as Mean
import qualified Statistics.EProcess.TwoSample as TS

-- $mean
--
-- For samples in @[lo, hi]@, test @H_0: E[x] = m@ two-sidedly.

-- | See 'Mean.config'.
meanConfig
  :: Double  -- ^ null mean @m@
  -> Double  -- ^ sample lower bound
  -> Double  -- ^ sample upper bound
  -> Double  -- ^ significance level @alpha@
  -> Bettor
  -> Mean.Config
meanConfig = Mean.config

-- | See 'Mean.initial'.
initMeanState :: Mean.Config -> Mean.State
initMeanState = Mean.initial

-- | See 'Mean.update'.
updateMean :: Mean.Config -> Mean.State -> Double -> Mean.State
updateMean = Mean.update

-- | See 'Mean.decide'.
decideMean :: Mean.Config -> Mean.State -> Mean.Verdict
decideMean = Mean.decide

-- $twosample
--
-- For paired observations @(a, b)@ both in @[lo, hi]@, test @H_0:
-- E[a] = E[b]@ two-sidedly.

-- | See 'TS.config'.
twoSampleConfig
  :: Double
  -> Double
  -> Double
  -> Bettor
  -> TS.Config
twoSampleConfig = TS.config

-- | See 'TS.initial'.
initTwoSampleState :: TS.Config -> TS.State
initTwoSampleState = TS.initial

-- | See 'TS.update'.
updateTwoSample
  :: TS.Config -> TS.State -> (Double, Double) -> TS.State
updateTwoSample = TS.update

-- | See 'TS.decide'.
decideTwoSample :: TS.Config -> TS.State -> TS.Verdict
decideTwoSample = TS.decide

-- | Current log-wealth of a 'Mean.State'.
logWealth :: Mean.State -> Double
logWealth = Mean.logWealth

-- | Sample count consumed so far.
samples :: Mean.State -> Int
samples = Mean.samples
