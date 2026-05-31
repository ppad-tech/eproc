{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Statistics.EProcess.Bettor
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bettor strategies for the e-process framework. A bettor maintains
-- internal state, consumes centred observations @z = x - m@, and
-- produces a predictable bet @lambda@ for the next observation.
--
-- The bet placed at step @t@ depends only on data observed through
-- step @t-1@; this predictability is what makes the resulting wealth
-- process a nonnegative supermartingale under the null hypothesis,
-- and hence anytime-valid via Ville's inequality.

module Statistics.EProcess.Bettor (
    -- * Bettor type
    Bettor(..)

    -- * Strategies
  , fixed
  , agrapa
  , ons

    -- * Strategy state types (opaque)
  , AGRAPA
  , ONS
  ) where

-- | A predictable bettor.
--
--   Parameterised over its internal state type @s@.
--
--   * @bettorInit@: initial state.
--   * @bettorStep@: update state with a newly observed centred
--     value @z = x - m@.
--   * @bettorBet@: bet @lambda@ to use for the /next/ observation,
--     given the current state.
data Bettor s = Bettor
  { bettorInit :: !s
  , bettorStep :: !(s -> Double -> s)
  , bettorBet  :: !(s -> Double)
  }

-- | Fixed-lambda bettor.
--
--   Always bets the same value. Useful for smoke-testing the
--   framework and as a numerical baseline.
fixed :: Double -> Bettor ()
fixed !lam = Bettor
  { bettorInit = ()
  , bettorStep = \_ _ -> ()
  , bettorBet  = \_ -> lam
  }

-- | aGRAPA bettor state (opaque).
data AGRAPA = AGRAPA
  { aSum  :: {-# UNPACK #-} !Double
  , aSum2 :: {-# UNPACK #-} !Double
  , aN    :: {-# UNPACK #-} !Int
  , aMax  :: {-# UNPACK #-} !Double
  }

-- | aGRAPA (approximate growth-rate adaptive predictable plug-in).
--
--   Tracks empirical mean and variance of centred observations @z@,
--   and bets the Kelly-optimal @lambda* = mu_z / (sigma_z^2 + mu_z^2)@
--   given the current point estimate, clipped to @[0, lambda_max]@.
--
--   The argument is @lambda_max@, the largest safe bet. For
--   observations @z = x - m@ where @x@ lies in @[lo, hi]@ and we are
--   testing @E[x] <= m@, a safe choice is @lambda_max = 1 \/ (m - lo)@
--   (so that the wealth factor @1 + lambda * z@ stays nonnegative).
agrapa :: Double -> Bettor AGRAPA
agrapa !lamMax = Bettor
  { bettorInit = AGRAPA 0 0 0 lamMax
  , bettorStep = \AGRAPA{..} !z ->
      AGRAPA (aSum + z) (aSum2 + z * z) (aN + 1) aMax
  , bettorBet  = \AGRAPA{..} ->
      if aN == 0
        then 0
        else
          let !n   = fromIntegral aN
              !mu  = aSum / n
              !mu2 = mu * mu
              !var = max 0 (aSum2 / n - mu2)
              !den = var + mu2
              !raw = if den == 0 then 0 else mu / den
          in  max 0 (min aMax raw)
  }

-- | ONS bettor state (opaque).
data ONS = ONS
  { onsLambda :: {-# UNPACK #-} !Double
  , onsAcc    :: {-# UNPACK #-} !Double
  , onsMax    :: {-# UNPACK #-} !Double
  }

-- | ONS (online Newton step) bettor.
--
--   Maintains a running sum of squared gradients of the per-step
--   log-wealth loss and updates @lambda@ by a Newton step at each
--   observation. Achieves logarithmic regret against the best
--   constant bet in hindsight; in practice the strongest of the
--   three bettors here under most signal regimes.
--
--   The argument is @lambda_max@; see 'agrapa' for the sizing rule.
ons :: Double -> Bettor ONS
ons !lamMax = Bettor
  { bettorInit = ONS 0 1.0e-6 lamMax
  , bettorStep = \ONS{..} !z ->
      let !denom = 1 + onsLambda * z
          !g     = if denom == 0 then 0 else negate z / denom
          !acc'  = onsAcc + g * g
          !lam'  = onsLambda - g / acc'
          !clp   = max 0 (min onsMax lam')
      in  ONS clp acc' onsMax
  , bettorBet  = onsLambda
  }
