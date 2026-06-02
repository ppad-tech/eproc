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
newtype Config = Config M.Config

-- | Test state.
newtype State = State M.State

-- | Build a paired two-sample test configuration.
--
--   Bounds @lo@ and @hi@ are the (shared) bounds on the individual
--   samples; differences then lie in @[lo - hi, hi - lo]@.
--
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.0 1.0 1.0e-6 B.Ons
config
  :: Double  -- ^ sample lower bound @lo@
  -> Double  -- ^ sample upper bound @hi@
  -> Double  -- ^ significance level @alpha@
  -> Bettor  -- ^ bettor strategy
  -> Config
config !lo !hi !alpha b =
  let !d = hi - lo
  in  Config (M.config 0 (negate d) d alpha b)
{-# INLINE config #-}

-- | Initial state for streaming.
initial :: Config -> State
initial (Config c) = State (M.initial c)
{-# INLINE initial #-}

-- | Fold one paired observation @(a, b)@ into the state.
update :: Config -> State -> (Double, Double) -> State
update (Config c) (State s) (!a, !b) =
  State (M.update c s (a - b))
{-# INLINE update #-}

-- | Decide based on current wealth.
decide :: Config -> State -> Verdict
decide (Config c) (State s) = M.decide c s
{-# INLINE decide #-}

-- | Current log-wealth.
logWealth :: State -> Double
logWealth (State s) = M.logWealth s
{-# INLINE logWealth #-}

-- | Sample count consumed so far.
samples :: State -> Int
samples (State s) = M.samples s
{-# INLINE samples #-}
