{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.ConfSeq.Bernoulli
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Anytime-valid confidence sequence for a Bernoulli rate, by
-- Robbins' method of mixtures.
--
-- For trials @x_t@ in @{0, 1}@, i.i.d. with rate @r@, after @s@
-- successes in @n@ trials, the likelihood ratio of the uniform
-- mixture over rates against @r@,
--
--     @M_n(r) = B(s + 1, n - s + 1) \/ (r^s (1 - r)^(n - s))@
--
-- (@B@ the beta function), is a nonnegative martingale under @r@
-- with @M_0(r) = 1@. By Ville's inequality it ever reaches
-- @1 \/ alpha@ with probability at most @alpha@, so the rates it has
-- not reached that against,
--
--     @C_n = { r : M_n(r) < 1 \/ alpha }@,
--
-- cover the true rate at every @n@ at once with probability at least
-- @1 - alpha@: inspect after every trial and stop when you like. As
-- @B(s + 1, n - s + 1) = 1 \/ ((n + 1) C(n, s))@,
--
--     @log M_n(r) = - log(n + 1) - log P(s; n, r)@,
--
-- @P@ the binomial probability, computed by Loader's saddle-point
-- expansion (as R's @dbinom@): stable at any count, where a
-- difference of log-factorials of size @n log n@ is not.
-- @log M_n@ is convex in @r@ and at most zero at @s \/ n@, so @C_n@
-- is an interval about @s \/ n@, its ends found by bisection; its
-- half-width is about
--
--     @sqrt(2 r (1 - r) (log(1 \/ alpha) + log(tau n r (1 - r)) \/ 2) \/ n)@
--
-- (@tau = 2 pi@), the price of anytime validity a term in @log n@.
--
-- Unlike "Numeric.Eproc.ConfSeq", which bets over a finite grid of
-- candidate means of any bounded variable, this is exact for a rate,
-- however small, and depends on the trials only through their
-- counts: 'from_counts' resumes from a tally kept elsewhere.
--
-- == Example
--
-- >>> let Right cfg = config 0.05
-- >>> let Just st = from_counts 30 100
-- >>> let (lo, hi) = interval cfg st
-- >>> lo < 0.3 && 0.3 < hi
-- True

module Numeric.Eproc.ConfSeq.Bernoulli (
  -- * Configuration and state
    Config
  , State
  , ConfigError(..)

  -- * Construction
  , config
  , initial
  , from_counts

  -- * Streaming
  , update

  -- * Inspection
  , interval
  , samples
  , successes
  ) where

import qualified Data.List as L
import Numeric (log1p)

import Numeric.Eproc.Common (ConfigError(..), finite)

-- | Confidence-sequence configuration.
data Config = Config {
    cfg_alpha      :: {-# UNPACK #-} !Double
  , cfg_log_thresh :: {-# UNPACK #-} !Double  -- log (1 / alpha)
  }

-- | The trials so far, as their counts.
data State = State {
    st_n :: {-# UNPACK #-} !Int
  , st_s :: {-# UNPACK #-} !Int
  }

-- | Build a 'Config' at a level in @(0, 1)@.
--
--   >>> either (const False) (const True) (config 0.05)
--   True
config :: Double -> Either ConfigError Config
config !alpha
  | not (finite alpha && alpha > 0 && alpha < 1) =
      Left (InvalidAlpha alpha)
  | otherwise = Right (Config alpha (log (1 / alpha)))
{-# INLINE config #-}

-- | The state before any trial.
initial :: Config -> State
initial _ = State 0 0
{-# INLINE initial #-}

-- | The state after some successes of some trials, where there are
--   no more successes than trials and neither is negative.
--
--   >>> maybe False (const True) (from_counts 3 2)
--   False
from_counts :: Int -> Int -> Maybe State
from_counts !s !n
  | s < 0 || n < s = Nothing
  | otherwise = Just (State n s)
{-# INLINE from_counts #-}

-- | The state after one more trial, a success or not.
update :: Config -> State -> Bool -> State
update _ (State n s) x = State (n + 1) (if x then s + 1 else s)
{-# INLINE update #-}

-- | The number of trials so far.
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}

-- | The number of successes so far.
successes :: State -> Int
successes = st_s
{-# INLINE successes #-}

-- | The confidence interval after the trials so far, @(0, 1)@ before
--   any. Each end is the outside one of the bracket a bisection
--   leaves it in, to the precision of a double, so that the interval
--   errs wide.
--
--   >>> let Right cfg = config 0.05
--   >>> interval cfg (initial cfg)
--   (0.0,1.0)
interval :: Config -> State -> (Double, Double)
interval Config{..} (State n s)
  | n <= 0 = (0, 1)
  | otherwise = (lower, upper)
  where
    !phat = fromIntegral s / fromIntegral n :: Double
    !ln1 = log (fromIntegral n + 1)
    inside r = negate ln1 - log_binom s n r < cfg_log_thresh
    lower
      | s == 0 = 0
      | otherwise = fst (bisect 0 phat)
    upper
      | s == n = 1
      | otherwise = snd (bisect phat 1)
    -- halving until the midpoint is one of the ends: the bracket as
    -- narrow as doubles allow, at any scale of rate
    bisect lo hi = go lo hi
      where
        !side = inside lo
        go x y =
          let !m = x + (y - x) / 2
          in  if m <= x || m >= y then (x, y)
              else if inside m == side then go m y else go x m

-- log P(s; n, r), the binomial probability of s successes in n trials
-- at rate r, by Loader's saddle-point expansion: Stirling's error
-- terms, the deviances of s from n r and of n - s from n (1 - r), and
-- the normal constant
log_binom :: Int -> Int -> Double -> Double
log_binom s n r
  | s == 0 = nN * log1p (negate r)
  | s == n = nN * log r
  | otherwise =
      stirlerr n - stirlerr s - stirlerr (n - s)
        - bd0 sN (nN * r) - bd0 fN (nN * (1 - r))
        + 0.5 * log (nN / (tau * sN * fN))
  where
    !nN = fromIntegral n :: Double
    !sN = fromIntegral s :: Double
    !fN = fromIntegral (n - s) :: Double

-- the deviance x log(x / np) + np - x, by its series where x is near
-- np, where the direct form cancels
bd0 :: Double -> Double -> Double
bd0 !x !np
  | abs (x - np) < 0.1 * (x + np) =
      let !v = (x - np) / (x + np)
          go !acc !ej !j =
            let !ej' = ej * v * v
                !acc' = acc + ej' / (2 * j + 1)
            in  if acc' == acc then acc' else go acc' ej' (j + 1)
      in  go ((x - np) * v) (2 * x * v) (1 :: Double)
  | otherwise = x * log (x / np) + np - x

-- Stirling's error, log k! - (k log k - k + log(tau k) / 2), for k at
-- least one: directly below 16, by its series above
stirlerr :: Int -> Double
stirlerr k
  | k < 16 = log_factorial k - (x * log x - x + 0.5 * log (tau * x))
  | k > 500 = (s0 - s1 / xx) / x
  | k > 80 = (s0 - (s1 - s2 / xx) / xx) / x
  | k > 35 = (s0 - (s1 - (s2 - s3 / xx) / xx) / xx) / x
  | otherwise = (s0 - (s1 - (s2 - (s3 - s4 / xx) / xx) / xx) / xx) / x
  where
    !x = fromIntegral k :: Double
    !xx = x * x
    s0 = 1 / 12
    s1 = 1 / 360
    s2 = 1 / 1260
    s3 = 1 / 1680
    s4 = 1 / 1188

-- log k!, for small k
log_factorial :: Int -> Double
log_factorial k = L.foldl' (\acc j -> acc + log (fromIntegral j)) 0 [2 .. k]

tau :: Double
tau = 2 * pi
