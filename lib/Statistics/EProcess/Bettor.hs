{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Statistics.EProcess.Bettor
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bettor strategies for the e-process framework. A bettor describes
-- how, given the history of centred observations @z = x - m@, the
-- next predictable bet @lambda@ is chosen.
--
-- The bet placed at step @t@ depends only on data observed through
-- step @t-1@; this predictability is what makes the resulting wealth
-- process a nonnegative supermartingale under the null hypothesis,
-- and hence anytime-valid via Ville's inequality.

module Statistics.EProcess.Bettor (
    -- * Bettor strategies
    Bettor(..)
  ) where

-- | A predictable bettor.
--
--   * 'Fixed' always bets the supplied @lambda@; useful for
--     smoke-testing the framework and as a numerical baseline.
--
--   * 'Agrapa' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor; tracks empirical mean and variance
--     of centred observations and bets the Kelly-optimal value given
--     the current point estimate, clipped to @[0, lambda_max]@.
--
--   * 'Ons' is the online Newton step bettor; maintains a running
--     sum of squared gradients of the per-step log-wealth loss and
--     updates @lambda@ by a Newton step at each observation. Achieves
--     logarithmic regret against the best constant bet in hindsight,
--     and is in practice the strongest of the three under most signal
--     regimes.
--
--   For 'Agrapa' and 'Ons', @lambda_max@ is derived from the sample
--   bounds supplied to the surrounding test 'Statistics.EProcess.Mean.config'.
data Bettor
  = Fixed {-# UNPACK #-} !Double
  | Agrapa
  | Ons
  deriving (Eq, Show)
