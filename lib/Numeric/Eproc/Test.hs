{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Numeric.Eproc.Test
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Paired two-sample anytime-valid mean-equality test.
--
-- For paired observations @(a_t, b_t)@ where both samples lie in
-- @[lo, hi]@, tests @H_0: E[a] = E[b]@ against
-- @H_1: E[a] /= E[b]@.
--
-- The reduction is straightforward: under the null, the differences
-- @d_t = a_t - b_t@ have mean zero, and differences of @[lo, hi]@
-- values lie in @[lo - hi, hi - lo]@. So the paired test is just
-- the bounded-mean test ("Numeric.Eproc.Mean") on @d_t@ with
-- null mean @0@ and sample bounds @[lo - hi, hi - lo]@.
--
-- Pairing is required: independent two-sample testing without
-- alignment would need to bet against a richer alternative (the
-- joint distribution rather than the marginal difference) and is
-- beyond the scope of this module.

module Numeric.Eproc.Test (
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

import qualified Numeric.Eproc.Mean as M
import Numeric.Eproc.Mean (Verdict(..))
import Numeric.Eproc.Bettor (Bettor)

-- types ----------------------------------------------------------------------

-- | Paired two-sample test configuration. Build with 'config'. Wraps
--   a 'Numeric.Eproc.Mean.Config' for the underlying
--   difference test.
newtype Config = Config M.Config

-- | Streaming paired two-sample test state. Construct with 'initial'
--   and fold paired observations through 'update'.
newtype State = State M.State

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the paired two-sample test.
--
--   Bounds @lo@ and @hi@ are the (shared) bounds on the individual
--   @a@ and @b@ samples; the underlying mean test is then configured
--   on the differences, which lie in @[lo - hi, hi - lo]@ with null
--   mean @0@.
--
--   >>> import qualified Numeric.Eproc.Bettor as B
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
--   Equivalent to feeding the difference @a - b@ into the underlying
--   bounded-mean test.
--
--   >>> let s1 = update cfg s0 (0.3, 0.7)
update :: Config -> State -> (Double, Double) -> State
update (Config c) (State s) (!a, !b) =
  State (M.update c s (a - b))
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff either directional log-wealth of the underlying
--   bounded-mean test on the differences has crossed
--   @log(2 \/ alpha)@.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide (Config c) (State s) = M.decide c s
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current log-wealth of the underlying bounded-mean test on
--   the differences.
--
--   >>> log_wealth s0
--   0.0
log_wealth :: State -> Double
log_wealth (State s) = M.log_wealth s
{-# INLINE log_wealth #-}

-- | The number of paired observations consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples (State s) = M.samples s
{-# INLINE samples #-}
