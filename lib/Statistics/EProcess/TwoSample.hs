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
-- @[lo, hi]@, tests @H_0: E[a] = E[b]@ against @H_1: E[a] /= E[b]@ by
-- running the bounded-mean test on the differences @d_t = a_t - b_t@
-- with null mean 0.

module Statistics.EProcess.TwoSample (
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

import qualified Statistics.EProcess.Mean as M
import Statistics.EProcess.Mean (Verdict(..))
import Statistics.EProcess.Bettor (Bettor)

-- types ----------------------------------------------------------------------

-- | Paired two-sample test configuration. Build with 'config'.
newtype Config = Config M.Config

-- | Streaming paired two-sample test state. Construct with 'initial'
--   and fold observations through 'update'.
newtype State = State M.State

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the paired two-sample test.
--
--   Bounds @lo@ and @hi@ are the (shared) bounds on the individual
--   samples; differences then lie in @[lo - hi, hi - lo]@.
--
--   >>> import qualified Statistics.EProcess.Bettor as B
--   >>> let cfg = config 0.0 1.0 1.0e-3 B.Ons
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

-- | The initial 'State' for a fresh streaming test.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial (Config c) = State (M.initial c)
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one paired observation @(a, b)@ into the running 'State'.
--
--   >>> let s1 = update cfg s0 (0.3, 0.7)
update :: Config -> State -> (Double, Double) -> State
update (Config c) (State s) (!a, !b) =
  State (M.update c s (a - b))
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide (Config c) (State s) = M.decide c s
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current log-wealth.
--
--   >>> log_wealth s0
--   0.0
log_wealth :: State -> Double
log_wealth (State s) = M.log_wealth s
{-# INLINE log_wealth #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples (State s) = M.samples s
{-# INLINE samples #-}
