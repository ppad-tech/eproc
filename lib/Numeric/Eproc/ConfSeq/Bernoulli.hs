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
-- @1 - alpha@: inspect after every trial and stop when you like.
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
--   leaves it in, so that the interval errs wide.
--
--   >>> let Right cfg = config 0.05
--   >>> interval cfg (initial cfg)
--   (0.0,1.0)
interval :: Config -> State -> (Double, Double)
interval Config{..} (State n s)
  | n <= 0 = (0, 1)
  | otherwise = (lower, upper)
  where
    !sN = fromIntegral s :: Double
    !fN = fromIntegral (n - s) :: Double
    !phat = sN / (sN + fN)
    !lb = log_factorial s + log_factorial (n - s) - log_factorial (n + 1)
    inside r =
      lb - times sN (log r) - times fN (log (1 - r)) < cfg_log_thresh
    -- k log x, nil where k is, whatever x
    times k x = if k == 0 then 0 else k * x
    lower
      | s == 0 = 0
      | otherwise = fst (bisect 0 phat)
    upper
      | s == n = 1
      | otherwise = snd (bisect phat 1)
    bisect lo hi = go (64 :: Int) lo hi
      where
        !side = inside lo
        go 0 x y = (x, y)
        go i x y =
          let !m = (x + y) / 2
          in  if inside m == side then go (i - 1) m y else go (i - 1) x m

-- log k!, summed below 256 and by Stirling's series above, where its
-- error is below 1e-12
log_factorial :: Int -> Double
log_factorial k
  | k < 2 = 0
  | k < 256 = L.foldl' (\acc j -> acc + log (fromIntegral j)) 0 [2 .. k]
  | otherwise =
      let !x = fromIntegral k :: Double
          !tau = 2 * pi
      in  x * log x - x + 0.5 * log (tau * x)
            + 1 / (12 * x) - 1 / (360 * x * x * x)
