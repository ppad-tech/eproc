{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Numeric.Eproc.Paired
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Paired two-sample anytime-valid mean-equality test.
--
-- For paired observations @(a_t, b_t)@ where both samples lie in
-- @[lo, hi]@, tests
--
--     @H_0: E[a_t - b_t | F_{t-1}] = 0   for all t@
--
-- against the negation. Here @F_{t-1}@ is the filtration generated
-- by everything observed strictly before time @t@; the conditional
-- form is what anytime validity actually requires. For i.i.d. pairs
-- this reduces to the usual marginal statement @E[a] = E[b]@; for
-- adaptively-collected or otherwise non-i.i.d. streams the
-- conditional statement is the right thing to think about.
--
-- The reduction is straightforward: under @H_0@, the differences
-- @d_t = a_t - b_t@ have (conditional) mean zero, and differences
-- of @[lo, hi]@ values lie in @[lo - hi, hi - lo]@. So the paired
-- test is just the bounded-mean test ("Numeric.Eproc.Bounded") on
-- @d_t@ with null mean @0@ and sample bounds @[lo - hi, hi - lo]@.
--
-- Pairing is required: independent two-sample testing without
-- alignment would need to bet against a richer alternative (the
-- joint distribution rather than the marginal difference) and is
-- beyond the scope of this module.
--
-- == Example
--
-- Test @H_0: E[a] = E[b]@ for samples in @[0, 1]@ at level
-- @alpha = 1e-3@ against a stream of paired observations where @a@
-- runs systematically higher than @b@:
--
-- >>> let Right cfg = config 0.0 1.0 1.0e-3 Newton
-- >>> let ps  = take 1000 (cycle [(1, 0), (1, 0), (0, 0), (1, 1)])
-- >>> decide cfg (foldl' (update cfg) (initial cfg) ps)
-- Reject

module Numeric.Eproc.Paired (
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

import qualified Numeric.Eproc.Bounded as Bounded
import Numeric.Eproc.Common (Bettor(..), Verdict(..), ConfigError(..))

-- types ----------------------------------------------------------------------

-- | Paired two-sample test configuration. Build with 'config'. Wraps
--   a 'Numeric.Eproc.Bounded.Config' for the underlying
--   difference test.
newtype Config = Config Bounded.Config

-- | Streaming paired two-sample test state. Construct with 'initial'
--   and fold paired observations through 'update'.
newtype State = State Bounded.State

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the paired two-sample test.
--
--   Bounds @lo@ and @hi@ are the (shared) bounds on the individual
--   @a@ and @b@ samples; the underlying mean test is then configured
--   on the differences, which lie in @[lo - hi, hi - lo]@ with null
--   mean @0@.
--
--   Returns 'Left' with a 'ConfigError' on inputs that would leave
--   the mathematical regime: any of @lo@, @hi@, @alpha@ non-finite
--   (NaN or infinite); @lo >= hi@; or @alpha@ outside @(0, 1)@.
--
--   >>> let Right cfg = config 0.0 1.0 1.0e-3 Newton
config
  :: Double  -- ^ sample lower bound @lo@
  -> Double  -- ^ sample upper bound @hi@
  -> Double  -- ^ significance level @alpha@
  -> Bettor  -- ^ bettor strategy
  -> Either ConfigError Config
config !lo !hi !alpha b =
  let !d = hi - lo
  in  fmap Config (Bounded.config 0 (negate d) d alpha b)
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial (Config c) = State (Bounded.initial c)
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one paired observation @(a, b)@ into the running 'State'.
--
--   Equivalent to feeding the difference @a - b@ into the underlying
--   bounded-mean test.
--
--   /Precondition/: both @a@ and @b@ must lie in the @[lo, hi]@
--   interval given to 'config'. The type-I error guarantee of the
--   test depends on this; the function does not check.
--
--   >>> let s1 = update cfg s0 (0.3, 0.7)
update :: Config -> State -> (Double, Double) -> State
update (Config c) (State s) (!a, !b) =
  State (Bounded.update c s (a - b))
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff either directional log-wealth of the underlying
--   bounded-mean test on the differences has /ever/ crossed
--   @log(2 \/ alpha)@.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide (Config c) (State s) = Bounded.decide c s
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The supremum-so-far log-wealth of the underlying bounded-mean
--   test on the differences.
--
--   >>> log_wealth s0
--   0.0
log_wealth :: State -> Double
log_wealth (State s) = Bounded.log_wealth s
{-# INLINE log_wealth #-}

-- | The number of paired observations consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples (State s) = Bounded.samples s
{-# INLINE samples #-}
