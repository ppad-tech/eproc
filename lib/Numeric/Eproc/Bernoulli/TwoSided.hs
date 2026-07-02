{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

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
-- This is exactly the two-sided bounded-mean test on @[0, 1]@ with
-- null mean @p_0@, so the module is a thin newtype wrapper over
-- "Numeric.Eproc.Bounded" (much as "Numeric.Eproc.Paired" is a
-- wrapper for the paired difference case). See the Bounded module
-- for the mathematical detail: convex-hedge combination of two
-- per-direction e-processes, threshold @log(2 \/ alpha)@, latched
-- rejection, etc.
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
  , log_wealth_sup
  , samples
  ) where

import qualified Numeric.Eproc.Bounded as Bounded
import Numeric.Eproc.Common (Bettor(..), Verdict(..), ConfigError(..))

-- types ----------------------------------------------------------------------

-- | Two-sided Bernoulli rate test configuration. Build with 'config'.
--   Wraps a 'Numeric.Eproc.Bounded.Config' on @[0, 1]@ with null
--   mean @p_0@.
newtype Config = Config Bounded.Config

-- | Streaming test state. Construct with 'initial' and fold
--   observations through 'update'.
newtype State = State Bounded.State

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for the two-sided Bernoulli rate test.
--
--   Returns 'Left' with a 'ConfigError' on inputs that would leave
--   the mathematical regime: @p_0@ outside @(0, 1)@ (or non-finite),
--   or @alpha@ outside @(0, 1)@ (or non-finite).
--
--   >>> let Right cfg = config 0.5 1.0e-3 Newton
config
  :: Double  -- ^ baseline rate @p_0@, in @(0, 1)@
  -> Double  -- ^ significance level @alpha@, in @(0, 1)@
  -> Bettor  -- ^ bettor strategy
  -> Either ConfigError Config
config !p0 !alpha b
  -- NaN comparisons return False and (-Inf, +Inf) fail the range
  -- check, so this catches non-finite p_0 without a separate guard.
  | not (p0 > 0 && p0 < 1) = Left (InvalidBaselineRate p0)
  | otherwise              = fmap Config (Bounded.config p0 0 1 alpha b)
{-# INLINE config #-}

-- | The initial 'State' for a fresh streaming test.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial (Config c) = State (Bounded.initial c)
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one observation into the running 'State'. Equivalent to
--   feeding the numeric @1@\/@0@ encoding of the observation into
--   the underlying bounded-mean test.
--
--   >>> let s1 = update cfg s0 True
update :: Config -> State -> Bool -> State
update (Config c) (State s) !x =
  State (Bounded.update c s (if x then 1 else 0))
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide (Config c) (State s) = Bounded.decide c s
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current @log(K^+_t + K^-_t)@ of the underlying bounded-mean
--   test. Not monotone; bounded above by 'log_wealth_sup'. Starts
--   at @log 2@.
--
--   >>> log_wealth s0
--   0.6931471805599453
log_wealth :: State -> Double
log_wealth (State s) = Bounded.log_wealth s
{-# INLINE log_wealth #-}

-- | The supremum-so-far of @log(K^+_t + K^-_t)@ from the underlying
--   bounded-mean test. Monotone nondecreasing; 'decide' rejects
--   exactly when it crosses @log(2 \/ alpha)@. Starts at @log 2@.
--
--   >>> log_wealth_sup s0
--   0.6931471805599453
log_wealth_sup :: State -> Double
log_wealth_sup (State s) = Bounded.log_wealth_sup s
{-# INLINE log_wealth_sup #-}

-- | The number of samples consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples (State s) = Bounded.samples s
{-# INLINE samples #-}
