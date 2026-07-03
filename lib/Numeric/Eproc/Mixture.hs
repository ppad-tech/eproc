{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.Mixture
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Uniform convex mixture of e-processes.
--
-- Given @K@ component e-processes @E^1_t, ..., E^K_t@ adapted to a
-- common filtration -- each testing (its facet of) a shared null
-- @H_0@ -- their arithmetic mean
--
--     @M_t = (E^1_t + ... + E^K_t) \/ K@
--
-- is itself an e-process with @M_0 = 1@: convex combinations
-- preserve the nonnegative-supermartingale property. By Ville's
-- inequality @P(sup_t M_t >= 1 \/ alpha) <= alpha@ under @H_0@, so a
-- level-@alpha@ test of the /combined/ null rejects when
-- @sup_t log(E^1_t + ... + E^K_t)@ crosses @log(K \/ alpha)@ -- no
-- Bonferroni correction, and strictly more powerful than one, since
-- the sum dominates the max. Use a mixture when the alternative has
-- several qualitatively different faces (a location shift, a shape
-- change, a rare-outlier channel, ...) and you want a single test
-- with power against their union.
--
-- This module does not own or update the components: they may be
-- heterogeneous (different test modules, different observation
-- transformations), so the caller steps each component itself and
-- feeds 'update' the vector of their current log e-values, as
-- reported by each module's @log_evalue@ accessor, one entry per
-- component in a fixed order.
--
-- Two preconditions are the caller's responsibility, and the
-- type-I guarantee depends on both:
--
--   1. Each entry must be the current log e-value of a genuine
--      e-process for @H_0@, and all components must be adapted to
--      the same filtration and stepped in lockstep -- 'update' is
--      called exactly once per underlying observation, after all
--      components have absorbed it.
--
--   2. The vector must have exactly the @K@ entries declared in
--      'config', always in the same order.
--
-- The rejection latch is kept on the supremum of the /mixture's/
-- log-wealth. Latching (or summing) per-component suprema instead
-- would combine peaks attained at different times -- a quantity
-- that can exceed anything the mixture ever reached, silently
-- inflating the effective alpha. Ville's inequality bounds the
-- mixture's own supremum; that is the only sound latch, and it is
-- the one this module maintains.
--
-- == Example
--
-- Combine a sign test and a magnitude test running against the same
-- stream of differences @d_t@ (the shape used for two-channel
-- symmetry testing):
--
-- >>> import qualified Numeric.Eproc.Bernoulli.TwoSided as Sign
-- >>> import qualified Numeric.Eproc.Bounded as Magn
-- >>> import qualified Numeric.Eproc.Mixture as Mix
-- >>> let Right sc = Sign.config 0.5 1.0e-3 Sign.Newton
-- >>> let Right mc = Magn.config 0.0 (-1.0) 1.0 1.0e-3 Magn.Newton
-- >>> let Right xc = Mix.config 2 1.0e-3
-- >>> :{
-- let step (s, m, x) d =
--       let s' = Sign.update sc s (d > 0)
--           m' = Magn.update mc m d
--       in  (s', m', Mix.update xc x
--                      [Sign.log_evalue s', Magn.log_evalue m'])
-- :}
-- >>> let ds = take 400 (cycle [0.6, 0.7, -0.2, 0.8])
-- >>> let z0 = (Sign.initial sc, Magn.initial mc, Mix.initial xc)
-- >>> let (_, _, xf) = foldl' step z0 ds
-- >>> Mix.decide xc xf
-- Reject
-- >>> Mix.p_value xc xf
-- 9.482234479673792e-34

module Numeric.Eproc.Mixture (
  -- * Mixture configuration and state
    Config
  , State
  , Verdict(..)
  , ConfigError(..)

  -- * Construction
  , config
  , initial

  -- * Streaming
  , update
  , decide

  -- * Inspection
  , log_wealth
  , log_wealth_sup
  , log_evalue
  , log_evalue_sup
  , p_value
  , samples
  ) where

import Numeric.Eproc.Common (Verdict(..), ConfigError(..), finite)

-- types ----------------------------------------------------------------------

-- | Mixture configuration. Build with 'config'.
--
--   Carries the component count @K@, the significance level, the
--   precomputed rejection threshold @log(K \/ alpha)@, and @log K@
--   (the mixture log-wealth of a fresh state).
data Config = Config {
    -- ^ component count @K@
    cfg_k          :: {-# UNPACK #-} !Int
    -- ^ significance level @alpha@
  , cfg_alpha      :: {-# UNPACK #-} !Double
    -- ^ rejection threshold @log(K \/ alpha)@
  , cfg_log_thresh :: {-# UNPACK #-} !Double
    -- ^ @log K@
  , cfg_log_k      :: {-# UNPACK #-} !Double
  }

-- | Streaming mixture state. Construct with 'initial' and fold
--   per-step component log e-value vectors through 'update'.
--
--   Tracks the current mixture log-wealth @log(sum_i E^i_t)@ and
--   its latched supremum, which is what 'decide' tests against the
--   rejection threshold.
data State = State {
    st_n           :: {-# UNPACK #-} !Int     -- ^ update count
  , st_log_sum     :: {-# UNPACK #-} !Double  -- ^ log(sum_i E^i)
  , st_sup_log_sum :: {-# UNPACK #-} !Double  -- ^ sup of the above
  }

-- construction ---------------------------------------------------------------

-- | Build a 'Config' for a @K@-component uniform mixture at level
--   @alpha@.
--
--   The rejection threshold is precomputed as @log(K \/ alpha)@:
--   the mixture @M_t = (sum_i E^i_t) \/ K@ crosses @1 \/ alpha@
--   exactly when the sum crosses @K \/ alpha@.
--
--   Returns 'Left' with a 'ConfigError' on inputs outside the
--   mathematical regime: @K < 1@, or @alpha@ non-finite or outside
--   @(0, 1)@.
--
--   >>> let Right cfg = config 4 1.0e-3
config
  :: Int     -- ^ component count @K@
  -> Double  -- ^ significance level @alpha@
  -> Either ConfigError Config
config !k !alpha
  | k < 1 =
      Left (InvalidComponentCount k)
  | not (finite alpha && alpha > 0 && alpha < 1) =
      Left (InvalidAlpha alpha)
  | otherwise =
      let !kd = fromIntegral k
      in  Right Config {
              cfg_k          = k
            , cfg_alpha      = alpha
            , cfg_log_thresh = log (kd / alpha)
            , cfg_log_k      = log kd
            }
{-# INLINE config #-}

-- | The initial 'State' for a fresh mixture.
--
--   Every component starts at e-value @1@, so the mixture log-sum
--   (and its supremum) starts at @log K@.
--
--   >>> let s0 = initial cfg
initial :: Config -> State
initial Config{..} = State {
    st_n           = 0
  , st_log_sum     = cfg_log_k
  , st_sup_log_sum = cfg_log_k
  }
{-# INLINE initial #-}

-- streaming ------------------------------------------------------------------

-- | Fold one step's component log e-values into the running
--   'State': computes the current mixture log-sum via a numerically
--   stable log-sum-exp and latches its supremum.
--
--   /Preconditions/ (documented in the module header, unchecked
--   here): the vector holds exactly the @K@ log e-values of
--   components adapted to a common filtration, in a fixed order,
--   with 'update' called once per underlying observation. The
--   degenerate empty vector leaves the state unchanged.
--
--   >>> let s1 = update cfg s0 [0.1, -0.2, 0.0, 0.4]
update :: Config -> State -> [Double] -> State
update _ st@State{..} les = case les of
  []       -> st
  (l : ls) ->
    let !m = foldl' max l ls
        !s = foldl' (\ !acc v -> acc + exp (v - m)) 0 les
        -- all components at e-value zero: the mixture log-sum is
        -- -Infinity, and (m +) would poison it into NaN.
        !cur | isInfinite m && m < 0 = m
             | otherwise             = m + log s
    in  State {
            st_n           = st_n + 1
          , st_log_sum     = cur
          , st_sup_log_sum = max st_sup_log_sum cur
          }
{-# INLINE update #-}

-- | Compute the current 'Verdict' from the running 'State'.
--
--   'Reject' iff the supremum-so-far of @log(sum_i E^i_t)@ has ever
--   crossed @log(K \/ alpha)@ -- equivalently, the mixture
--   e-process @M_t@ has exceeded @1 \/ alpha@ at some point in the
--   stream so far. Under the combined @H_0@, by Ville's inequality,
--   the probability of this ever happening is at most @alpha@,
--   simultaneously over all sample sizes: peek and stop freely.
--
--   >>> decide cfg s0
--   Continue
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_sup_log_sum >= cfg_log_thresh = Reject
  | otherwise                        = Continue
{-# INLINE decide #-}

-- inspection -----------------------------------------------------------------

-- | The current mixture log-wealth @log(sum_i E^i_t)@, before
--   normalization by @K@. Not monotone; bounded above by
--   'log_wealth_sup'. Starts at @log K@.
--
--   >>> log_wealth s0
--   1.3862943611198906
log_wealth :: State -> Double
log_wealth = st_log_sum
{-# INLINE log_wealth #-}

-- | The supremum-so-far of @log(sum_i E^i_t)@. Monotone
--   nondecreasing; 'decide' rejects exactly when it crosses
--   @log(K \/ alpha)@. Starts at @log K@.
--
--   >>> log_wealth_sup s0
--   1.3862943611198906
log_wealth_sup :: State -> Double
log_wealth_sup = st_sup_log_sum
{-# INLINE log_wealth_sup #-}

-- | The current log e-value of the mixture: the log of
--   @M_t = (sum_i E^i_t) \/ K@, i.e. 'log_wealth' minus @log K@,
--   normalized so a fresh state sits at @0@. This is itself a
--   component-shaped quantity: mixtures nest, so it can in turn be
--   fed to an outer mixture. Not monotone; bounded above by
--   'log_evalue_sup'.
--
--   >>> log_evalue s0
--   0.0
log_evalue :: Config -> State -> Double
log_evalue Config{..} State{..} = st_log_sum - cfg_log_k
{-# INLINE log_evalue #-}

-- | The supremum-so-far of the log e-value: 'log_wealth_sup' minus
--   @log K@. Monotone nondecreasing, starting at @0@; 'decide'
--   rejects exactly when it crosses @log(1 \/ alpha)@.
--
--   >>> log_evalue_sup s0
--   0.0
log_evalue_sup :: Config -> State -> Double
log_evalue_sup Config{..} State{..} = st_sup_log_sum - cfg_log_k
{-# INLINE log_evalue_sup #-}

-- | The anytime-valid p-value: the reciprocal of the largest
--   mixture e-value attained so far. Monotone nonincreasing; under
--   the combined @H_0@, @P(exists t: p_t <= alpha) <= alpha@ for
--   every @alpha@ simultaneously. 'decide' returns 'Reject' exactly
--   when this value has reached the configured @alpha@ or below.
--
--   >>> p_value cfg s0
--   1.0
p_value :: Config -> State -> Double
p_value cfg s = min 1 (exp (negate (log_evalue_sup cfg s)))
{-# INLINE p_value #-}

-- | The number of 'update' steps consumed so far.
--
--   >>> samples s0
--   0
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
