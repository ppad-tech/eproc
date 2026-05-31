{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Statistics.EProcess.TwoSample
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Paired two-sample anytime-valid mean-equality test.
--
-- For paired observations @(a_t, b_t)@ where both samples lie in
-- @[lo, hi]@, tests @H_0: E[a] = E[b]@ against @H_1: E[a] /= E[b]@
-- by running the bounded-mean test on the differences @d_t = a_t -
-- b_t@ with null mean 0.

module Statistics.EProcess.TwoSample (
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

import qualified Statistics.EProcess.Mean as M
import Statistics.EProcess.Mean (Verdict(..))
import Statistics.EProcess.Bettor (Bettor)

-- | Test configuration.
newtype Config s = Config (M.Config s)

-- | Test state.
newtype State s = State (M.State s)

-- | Build a paired two-sample test configuration.
--
--   Bounds @lo@ and @hi@ are the (shared) bounds on the individual
--   samples; differences then lie in @[lo - hi, hi - lo]@.
--
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.0 1.0 1.0e-6 B.ons
config
  :: Double                -- ^ sample lower bound @lo@
  -> Double                -- ^ sample upper bound @hi@
  -> Double                -- ^ significance level @alpha@
  -> (Double -> Bettor s)  -- ^ bettor builder
  -> Config s
config !lo !hi !alpha mk =
  let !b = hi - lo
  in  Config (M.config 0 (negate b) b alpha mk)

-- | Initial state for streaming.
initial :: Config s -> State s
initial (Config c) = State (M.initial c)

-- | Fold one paired observation @(a, b)@ into the state.
update :: Config s -> State s -> (Double, Double) -> State s
update (Config c) (State s) (!a, !b) =
  State (M.update c s (a - b))

-- | Decide based on current wealth.
decide :: Config s -> State s -> Verdict
decide (Config c) (State s) = M.decide c s

-- | Current log-wealth.
logWealth :: State s -> Double
logWealth (State s) = M.logWealth s

-- | Sample count consumed so far.
samples :: State s -> Int
samples (State s) = M.samples s
