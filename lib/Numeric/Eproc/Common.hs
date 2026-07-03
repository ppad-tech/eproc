{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Numeric.Eproc.Common
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Shared vocabulary for the eproc tests: the predictable bettor
-- strategies, the test verdict type, and the configuration-error
-- type. Re-exported from each test module
-- ("Numeric.Eproc.Bounded", "Numeric.Eproc.Paired",
-- "Numeric.Eproc.Bernoulli"); import this module directly only if
-- you need the types without picking a particular test.
--
-- The 'BetState' type and its helpers are internal to the library:
-- they are exposed here so that 'Numeric.Eproc.Bounded' and
-- 'Numeric.Eproc.Bernoulli' can share one implementation, not for
-- direct use.

module Numeric.Eproc.Common (
    Bettor(..)
  , Verdict(..)
  , ConfigError(..)

  -- * Internal: shared bettor state
  , BetState(..)
  , init_bet
  , bet_lambda
  , step_bet

  -- * Internal: helpers
  , finite
  , log_sum_exp
  , log2_dbl
  ) where

import GHC.Float (log1p)

-- | A predictable bettor.
--
--   A bettor describes how, given the history of centred
--   observations @z_t@ (each test module specifies its own centring;
--   see the per-module documentation), the next predictable bet
--   @lambda_t@ is chosen. Predictability -- that is, @lambda_t@
--   depends only on data observed strictly before step @t@ -- is
--   what makes the resulting wealth process a nonnegative
--   supermartingale under @H_0@.
--
--   All three bettors enforce a safe-bet ceiling @lambda_max@
--   derived from the test's admissible-observation range by clipping
--   @lambda@ to @[0, lambda_max]@; this keeps the per-step wealth
--   factor nonnegative.
--
--   * 'Fixed' bets the supplied constant @lambda@ (clipped to
--     @[0, lambda_max]@). The wager does not respond to observed
--     data; this strategy is useful only as a baseline.
--
--   * 'Adaptive' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor of Waudby-Smith & Ramdas (2024).
--     It tracks the empirical mean @mu@ and variance @sigma^2@ of
--     centred observations and bets the Kelly-optimal plug-in
--     @lambda* = mu \/ (sigma^2 + mu^2)@ clipped to
--     @[0, lambda_max]@. Fast to compute and competitive in
--     practice.
--
--   * 'Newton' is the online Newton step (ONS) bettor of
--     Waudby-Smith & Ramdas (2024, Algorithm 2). The per-step
--     log-wealth loss @-log(1 + lambda * z)@ is convex in @lambda@;
--     ONS performs one Newton step per observation, accumulating
--     squared gradients to scale the update by a fixed learning
--     rate @2 \/ (2 - log 3)@. Achieves logarithmic regret against
--     the best constant bet in hindsight and is in practice the
--     strongest of the three bettors under most signal regimes.
--
--     One deliberate deviation from WSR: Algorithm 2 seeds the
--     squared-gradient accumulator at @1@, which presumes
--     observations scaled to @[0, 1]@. On raw-scale data that
--     constant is dimensionally wrong -- negligible when
--     @z^2 >> 1@, paralysing when @z^2 << 1@ -- so the accumulator
--     here is instead seeded near zero, making the update
--     scale-adaptive. The trade is bold early play: the first
--     nonzero observation typically drives the bet straight to
--     the @lambda_max@ ceiling, annealing back toward the Kelly
--     point as gradients accumulate. Validity is unaffected --
--     predictability and clipping are all it needs -- and regret
--     stays logarithmic with a somewhat larger constant. The
--     visible effect is higher-variance early wealth: a supremum
--     modestly above its floor is expected even under @H_0@.
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Adaptive
  | Newton
  deriving (Eq, Show)

-- | Test outcome at the current sample count.
--
--   'Reject' means the wealth process has /ever/ crossed the
--   rejection threshold, so @H_0@ is rejected at level @alpha@.
--   Once a state has rejected it stays rejected, even if subsequent
--   observations drive the current wealth back below threshold;
--   this is the supremum-style guarantee that Ville's inequality
--   actually delivers. 'Continue' means there is not yet enough
--   evidence; collect more samples (or stop and report no
--   rejection -- the type-I error guarantee holds for /any/
--   stopping rule).
data Verdict =
    Reject
  | Continue
  deriving (Eq, Show)

-- | Reasons that a test-configuration smart constructor can reject
--   its inputs. Returned by 'Numeric.Eproc.Bounded.config',
--   'Numeric.Eproc.Bernoulli.config',
--   'Numeric.Eproc.Paired.config', and
--   'Numeric.Eproc.Mixture.config'.
data ConfigError =
    -- | significance level outside @(0, 1)@
    InvalidAlpha {-# UNPACK #-} !Double
    -- | sample bounds violate @lo < hi@
  | InvalidBounds {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    -- | null mean outside @(lo, hi)@ (strict, to avoid div-by-zero
    --   in the safe-bet ceilings)
  | InvalidNullMean
      {-# UNPACK #-} !Double  -- m
      {-# UNPACK #-} !Double  -- lo
      {-# UNPACK #-} !Double  -- hi
    -- | baseline rate outside @(0, 1)@
  | InvalidBaselineRate {-# UNPACK #-} !Double
    -- | component count not positive
  | InvalidComponentCount {-# UNPACK #-} !Int
  deriving (Eq, Show)

-- | True iff the argument is a finite IEEE-754 double (not NaN, not
--   @+\/-Infinity@). Used by the @config@ smart constructors to keep
--   the bounded-random-variable promise honest.
finite :: Double -> Bool
finite x = not (isNaN x) && not (isInfinite x)
{-# INLINE finite #-}

-- | @log(exp a + exp b)@, computed without intermediate overflow.
--   Used by the convex-hedge two-sided combinations to update the
--   running @log(K^+ + K^-)@ statistic from the two per-direction
--   log-wealths.
log_sum_exp :: Double -> Double -> Double
log_sum_exp !a !b
  | a >= b    = a + log1p (exp (b - a))
  | otherwise = b + log1p (exp (a - b))
{-# INLINE log_sum_exp #-}

-- | @log 2@ as a shared constant. Used both as the initial value of
--   the two-sided running sup-log-sum (since @K^+_0 + K^-_0 = 2@) and
--   as the tight upper-bound slack in the fast-path skip inside
--   'Numeric.Eproc.Bounded.update' /
--   'Numeric.Eproc.Bernoulli.TwoSided.update'.
log2_dbl :: Double
log2_dbl = log 2
{-# INLINE log2_dbl #-}

-- | Per-bettor state. One constructor per 'Bettor' alternative; the
--   constructor used in any given state matches the 'Bettor' chosen
--   in the enclosing 'Config'.
--
--   Internal: exposed only so that the per-test 'State' types in
--   "Numeric.Eproc.Bounded" and "Numeric.Eproc.Bernoulli" can share
--   one implementation.
data BetState =
    SFixed
  | SAdaptive
      {-# UNPACK #-} !Double  -- sum of z (centred observation)
      {-# UNPACK #-} !Double  -- sum of z^2 (for online variance)
      {-# UNPACK #-} !Int     -- count
  | SNewton
      {-# UNPACK #-} !Double  -- current bet lambda
      {-# UNPACK #-} !Double  -- running sum of per-step squared gradients

-- | Per-bettor initial state.
init_bet :: Bettor -> BetState
init_bet b = case b of
  Fixed _  -> SFixed
  Adaptive -> SAdaptive 0 0 0
  Newton   -> SNewton 0 1.0e-6  -- small acc seed avoids div-by-zero
{-# INLINE init_bet #-}

-- | WSR (2024) Algorithm 2 ONS learning rate, @2 \/ (2 - log 3)@.
ons_lr :: Double
ons_lr = 2 / (2 - log 3)
{-# INLINE ons_lr #-}

-- | Compute the next bet 'lambda' from the bettor and its current
--   state; 'lam_max' is the direction-specific safety bound. All
--   strategies clip the result to @[0, lam_max]@ so the wealth
--   factor stays nonnegative.
bet_lambda :: Bettor -> Double -> BetState -> Double
bet_lambda b !lam_max !s = case b of
  Fixed lam -> max 0 (min lam_max lam)
  Adaptive -> case s of
    SAdaptive !sm !sm2 !n
      | n == 0    -> 0
      | otherwise ->
          let !nd  = fromIntegral n
              !mu  = sm / nd
              !mu2 = mu * mu
              !var = max 0 (sm2 / nd - mu2)
              !den = var + mu2
              !raw = if den == 0 then 0 else mu / den
          in  max 0 (min lam_max raw)
    _ -> 0
  Newton -> case s of
    SNewton !lam _ -> lam
    _              -> 0
{-# INLINE bet_lambda #-}

-- | Update bettor state with newly observed centred value 'z'. For
--   'Adaptive' this is just accumulating sums; for 'Newton' we take
--   one online Newton step (with the WSR learning rate) on the
--   per-step log-wealth loss @-log(1 + lambda * z)@, accumulating
--   squared gradients for adaptive scaling.
step_bet :: Bettor -> Double -> BetState -> Double -> BetState
step_bet b !lam_max !s !z = case b of
  Fixed _ -> SFixed
  Adaptive -> case s of
    SAdaptive !sm !sm2 !n -> SAdaptive (sm + z) (sm2 + z * z) (n + 1)
    _                     -> SAdaptive z (z * z) 1
  Newton -> case s of
    SNewton !lam !acc ->
      let !denom = 1 + lam * z
          !g     = if denom == 0 then 0 else negate z / denom
          !acc'  = acc + g * g
          !lam'  = lam - ons_lr * g / acc'
          !clp   = max 0 (min lam_max lam')
      in  SNewton clp acc'
    _ -> SNewton 0 1.0e-6
{-# INLINE step_bet #-}
