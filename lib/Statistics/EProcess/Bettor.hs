{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Statistics.EProcess.Bettor
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bettor strategies for the e-process framework.
--
-- A bettor describes how, given the history of centred observations
-- @z = x - m@, the next predictable bet @lambda@ is chosen. The bet
-- placed at step @t@ depends only on data observed through step @t-1@;
-- this predictability is what makes the resulting wealth process a
-- nonnegative supermartingale under the null hypothesis, and hence
-- anytime-valid via Ville's inequality.

module Statistics.EProcess.Bettor (
  -- * Bettor strategies
    Bettor(..)
  ) where

-- bettor strategies ----------------------------------------------------------

-- | A predictable bettor.
--
--   For 'Agrapa' and 'Ons', the safe bet bound @lambda_max@ is derived
--   from the sample bounds supplied to the surrounding test
--   configuration (e.g. 'Statistics.EProcess.Mean.config').
--
--   * 'Fixed' always bets the supplied @lambda@; useful for smoke
--     testing the framework and as a numerical baseline.
--
--   * 'Agrapa' is the aGRAPA (approximate growth-rate adaptive
--     predictable plug-in) bettor. Tracks empirical mean and variance
--     of centred observations and bets the Kelly-optimal value given
--     the current point estimate, clipped to @[0, lambda_max]@.
--
--   * 'Ons' is the online Newton step bettor. Maintains a running
--     sum of squared gradients of the per-step log-wealth loss and
--     updates @lambda@ by a Newton step at each observation; achieves
--     logarithmic regret against the best constant bet in hindsight,
--     and is in practice the strongest of the three bettors under most
--     signal regimes.
--
--   >>> Fixed 0.5
--   Fixed 0.5
--   >>> Agrapa
--   Agrapa
--   >>> Ons
--   Ons
data Bettor =
    Fixed {-# UNPACK #-} !Double
  | Agrapa
  | Ons
  deriving (Eq, Show)
