{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Numeric.Eproc.Bettor
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bettor strategies for the e-process framework.
--
-- A bettor describes how, given the history of centred observations
-- @z_t = x_t - m@ (where @x_t@ is the new observation and @m@ is the
-- null mean), the next predictable bet @lambda_t@ is chosen. The
-- wealth process is the running product of per-step factors
--
--     @W_t = prod_{s <= t} (1 + lambda_s * z_s)@
--
-- and the test rejects when @W_t@ crosses @1\/alpha@. Predictability
-- -- that is, @lambda_t@ depends only on data observed strictly
-- before step @t@ -- is what makes @W@ a nonnegative supermartingale
-- under @H_0@, so that Ville's inequality applies and the resulting
-- test is anytime-valid.

module Numeric.Eproc.Bettor (
  -- * Bettor strategies
    Bettor(..)
  ) where

-- bettor strategies ----------------------------------------------------------

-- | A predictable bettor.
--
--   For 'Agrapa' and 'Ons', a per-direction safe-bet ceiling
--   @lambda_max@ is derived from the sample bounds supplied to the
--   surrounding test configuration (e.g.
--   'Numeric.Eproc.Bounded.config') -- bets get clipped to
--   @[0, lambda_max]@ so that the wealth factor @1 + lambda * z@
--   stays nonnegative for every admissible observation.
--
--   * 'Fixed' always bets the supplied constant @lambda@. The wager
--     does not respond to observed data; this strategy is useful only
--     as a baseline.
--
--   * 'Agrapa' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor of Waudby-Smith & Ramdas (2024).
--     It tracks the empirical mean @mu@ and variance @sigma^2@ of
--     centred observations and bets the Kelly-optimal plug-in
--     @lambda* = mu \/ (sigma^2 + mu^2)@ clipped to
--     @[0, lambda_max]@. Fast to compute and competitive in practice.
--
--   * 'Ons' is the online Newton step bettor. The per-step log-wealth
--     loss @-log(1 + lambda * z)@ is convex in @lambda@; ONS performs
--     one Newton step per observation, accumulating squared gradients
--     to scale the update. Achieves logarithmic regret against the
--     best constant bet in hindsight and is in practice the strongest
--     of the three bettors under most signal regimes.
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Agrapa
  | Ons
  deriving (Eq, Show)
