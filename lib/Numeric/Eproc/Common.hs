{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Numeric.Eproc.Common
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Shared vocabulary for the eproc tests: the predictable bettor
-- strategies and the test verdict type. Re-exported from each test
-- module ("Numeric.Eproc.Bounded", "Numeric.Eproc.Paired",
-- "Numeric.Eproc.Bernoulli"); import this module directly only if
-- you need the types without picking a particular test.

module Numeric.Eproc.Common (
    Bettor(..)
  , Verdict(..)
  ) where

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
--   For 'Adaptive' and 'Newton', a safe-bet ceiling @lambda_max@
--   derived from the test's admissible-observation range is enforced
--   by clipping @lambda@ to @[0, lambda_max]@, so the wealth factor
--   stays nonnegative.
--
--   * 'Fixed' always bets the supplied constant @lambda@. The wager
--     does not respond to observed data; this strategy is useful
--     only as a baseline.
--
--   * 'Adaptive' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor of Waudby-Smith & Ramdas (2024).
--     It tracks the empirical mean @mu@ and variance @sigma^2@ of
--     centred observations and bets the Kelly-optimal plug-in
--     @lambda* = mu \/ (sigma^2 + mu^2)@ clipped to
--     @[0, lambda_max]@. Fast to compute and competitive in
--     practice.
--
--   * 'Newton' is the online Newton step (ONS) bettor. The per-step
--     log-wealth loss @-log(1 + lambda * z)@ is convex in @lambda@;
--     ONS performs one Newton step per observation, accumulating
--     squared gradients to scale the update. Achieves logarithmic
--     regret against the best constant bet in hindsight and is in
--     practice the strongest of the three bettors under most signal
--     regimes.
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Adaptive
  | Newton
  deriving (Eq, Show)

-- | Test outcome at the current sample count.
--
--   'Reject' means the wealth process has crossed the rejection
--   threshold, so @H_0@ is rejected at level @alpha@. 'Continue'
--   means there is not yet enough evidence; collect more samples
--   (or stop and report no rejection -- the type-I error guarantee
--   holds for /any/ stopping rule).
data Verdict =
    Reject
  | Continue
  deriving (Eq, Show)
