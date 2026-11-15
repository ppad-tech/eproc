{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.Eproc.TwoSample
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Anytime-valid two-sample test of equality in law, by a hedge of
-- e-processes on an antisymmetric statistic.
--
-- Draw pairs @(a_t, b_t)@, one from each of two laws, independently.
-- Under the null that the laws are equal the pair is exchangeable,
-- so any antisymmetric function of it, @f(a, b) = -f(b, a)@, is
-- symmetric about zero, whatever the law. The hedge bets on several
-- at once, each a component e-process for the null:
--
--   * the sign of the difference @d = a - b@ (ties skipped), against
--     one law running above the other ("Numeric.Eproc.Bernoulli.TwoSided");
--
--   * @d@ clipped at each of some bounds, against a shift in mean,
--     without letting rare large differences dominate
--     ("Numeric.Eproc.Bounded");
--
--   * at each of some cut points @q@, @1[a <= q] - 1[b <= q]@, against
--     the laws' distribution functions differing at @q@ (a change in
--     spread, say, that leaves the mean alone).
--
-- The hedge's e-value is the components' mean ("Numeric.Eproc.Merge"),
-- an e-process for the null; the test rejects when its supremum so
-- far reaches @1 \/ alpha@, with probability at most @alpha@ under the
-- null, at any stopping time. The hedge's price is the log of its
-- number of components, against power on each.
--
-- Nothing ties the statistic to a pair. 'update' takes any statistic
-- symmetric about zero under the null, and one per cut point bounded
-- by a given bound: the energy of a quartet (two draws from each of
-- two laws), say, which swapping two of its draws negates. 'update_pair'
-- makes the statistics of a pair.
--
-- Under an interval null ('configInterval'), that the clipped
-- difference's mean lies within a margin of zero, the clipped
-- components bet against the interval and alone make the hedge's
-- e-value; the sign and indicator components test the sharp null,
-- and are stepped beside it but not mixed.
--
-- The clip bounds and cut points must not depend on the data the
-- test sees: fix them from a warmup the test then discards
-- ('clip_bound', 'cut_points').
--
-- == Example
--
-- >>> let Right cfg = config 1.0e-3 [1.0, 4.0] 1 [0.5]
-- >>> let ps = take 2000 (cycle [(1, 0), (1, 0), (0, 0), (1, 1)])
-- >>> decide cfg (foldl' (update_pair cfg) (initial cfg) ps)
-- Reject

module Numeric.Eproc.TwoSample (
  -- * Configuration and state
    Config
  , State
  , Verdict(..)
  , ConfigError(..)

  -- * Construction
  , config
  , configInterval
  , initial

  -- * Calibration
  , clip_bound
  , cut_points

  -- * Streaming
  , update
  , update_pair
  , decide

  -- * Inspection
  , log_evalue
  , log_evalue_sup
  , log_evalue_sign
  , log_evalue_magnitude
  , log_evalue_cdf
  , samples
  ) where

import qualified Data.List as L

import qualified Numeric.Eproc.Bernoulli.TwoSided as Sign
import qualified Numeric.Eproc.Bounded as Magn
import Numeric.Eproc.Common (Bettor(..), ConfigError(..), Verdict(..),
  finite)
import Numeric.Eproc.Merge (log_mean)

-- | Hedge configuration.
data Config = Config {
    cfg_alpha      :: {-# UNPACK #-} !Double
  , cfg_log_thresh :: {-# UNPACK #-} !Double  -- log (1 / alpha)
  , cfg_sign       :: !Sign.Config
  , cfg_magn       :: ![(Double, Magn.Config)]  -- each clip bound, its
                                                -- test
  , cfg_cdf_bound  :: {-# UNPACK #-} !Double
  , cfg_cdf        :: !Magn.Config               -- shared by every cut
  , cfg_cuts       :: ![Double]
  , cfg_interval   :: !Bool                      -- an interval null
  }

-- | The hedge after some observations.
data State = State {
    st_n       :: {-# UNPACK #-} !Int
  , st_sign    :: !Sign.State
  , st_magn    :: ![Magn.State]
  , st_cdf     :: ![Magn.State]
  , st_sup     :: {-# UNPACK #-} !Double  -- the log e-value's supremum
  }

-- | Build a 'Config' for the sharp null, at a level in @(0, 1)@, from
--   positive clip bounds, the positive bound of the indicator
--   statistics (one for a pair's), and the cut points.
--
--   >>> either (const False) (const True) (config 1.0e-3 [1, 4] 1 [0])
--   True
config
  :: Double    -- ^ significance level @alpha@
  -> [Double]  -- ^ clip bounds
  -> Double    -- ^ indicator statistics' bound
  -> [Double]  -- ^ cut points
  -> Either ConfigError Config
config = mk Nothing
{-# INLINE config #-}

-- | Build a 'Config' for the interval null that the clipped
--   difference's mean lies within a positive margin of zero; each
--   clip bound must exceed the margin.
configInterval
  :: Double    -- ^ margin
  -> Double    -- ^ significance level @alpha@
  -> [Double]  -- ^ clip bounds
  -> Double    -- ^ indicator statistics' bound
  -> [Double]  -- ^ cut points
  -> Either ConfigError Config
configInterval m = mk (Just m)
{-# INLINE configInterval #-}

mk
  :: Maybe Double -> Double -> [Double] -> Double -> [Double]
  -> Either ConfigError Config
mk margin !alpha bs !u qs
  | not (finite alpha && alpha > 0 && alpha < 1) =
      Left (InvalidAlpha alpha)
  | not (finite u && u > 0) = Left (InvalidBounds (negate u) u)
  | otherwise = do
      s <- Sign.config 0.5 alpha Newton
      ms <- traverse magn bs
      cd <- Magn.config 0 (negate u) u alpha Newton
      pure Config {
          cfg_alpha      = alpha
        , cfg_log_thresh = log (1 / alpha)
        , cfg_sign       = s
        , cfg_magn       = ms
        , cfg_cdf_bound  = u
        , cfg_cdf        = cd
        , cfg_cuts       = qs
        , cfg_interval   = maybe False (const True) margin
        }
  where
    magn !b
      | not (finite b && b > 0) = Left (InvalidBounds (negate b) b)
      | otherwise = fmap ((,) b) $ case margin of
          Nothing -> Magn.config 0 (negate b) b alpha Newton
          Just m -> Magn.configInterval (negate m) m (negate b) b alpha
            Newton

-- | The hedge before any observation, its e-value one.
initial :: Config -> State
initial Config{..} = State {
    st_n    = 0
  , st_sign = Sign.initial cfg_sign
  , st_magn = strict_map (Magn.initial . snd) cfg_magn
  , st_cdf  = strict_map (const (Magn.initial cfg_cdf)) cfg_cuts
  , st_sup  = 0
  }

-- a map whose list is whole, spine and elements, when evaluated at
-- all, as update's are
strict_map :: (a -> b) -> [a] -> [b]
strict_map f = go
  where
    go (x : xs) = let !y = f x
                      !ys = go xs
                  in  y : ys
    go [] = []
{-# INLINE initial #-}

-- calibration ---------------------------------------------------------------

-- | A clip bound from a warmup's statistics: twice the nearest-rank
--   p99 of their magnitudes (the @ceiling (0.99 m)@-th smallest of
--   @m@), and at least a floor.
--
--   >>> clip_bound 1 [0.1, 0.2, -0.3]
--   1.0
--   >>> clip_bound 0 (replicate 99 1 ++ [1.0e9])
--   2.0
clip_bound :: Double -> [Double] -> Double
clip_bound !floor_ ds = max floor_ (2 * nearest 0.99 (L.sort (fmap abs ds)))
{-# INLINE clip_bound #-}

-- | Cut points from a warmup's pooled values: their nearest-rank
--   quantiles at 0.10, 0.25, 0.50, 0.75 and 0.90, ascending, alike
--   ones once. None from no values.
--
--   >>> cut_points [1, 2, 3, 4]
--   [1.0,2.0,3.0,4.0]
cut_points :: [Double] -> [Double]
cut_points xs = case L.sort xs of
  [] -> []
  sorted -> dedup [ nearest q sorted | q <- [0.10, 0.25, 0.50, 0.75, 0.90] ]
  where
    dedup (a : b : rest)
      | a == b = dedup (b : rest)
      | otherwise = a : dedup (b : rest)
    dedup ys = ys

-- the nearest-rank order statistic at a level of a sorted list: its
-- ceiling (q m)-th smallest of m, zero of an empty list
nearest :: Double -> [Double] -> Double
nearest !q xs = case drop i xs of
  x : _ -> x
  [] -> 0
  where
    !m = length xs
    !i = max 0 (min (m - 1) (ceiling (q * fromIntegral m) - 1))

-- streaming -----------------------------------------------------------------

-- | The hedge after one more observation: a statistic symmetric about
--   zero under the null, and one indicator statistic per cut point,
--   within the indicators' bound (clipped to it otherwise).
--
--   /Precondition/: one indicator statistic per cut point, in their
--   order; any beyond them are ignored.
update :: Config -> State -> Double -> [Double] -> State
update cfg@Config{..} State{..} !d cs =
  let !sg | d == 0 = st_sign
          | otherwise = Sign.update cfg_sign st_sign (d > 0)
      !mg = step_magn cfg_magn st_magn
      !cd = step_cdf cs st_cdf
      st' = State (st_n + 1) sg mg cd st_sup
      !le = log_evalue cfg st'
  in  st' { st_sup = max st_sup le }
  where
    step_magn ((b, c) : bt) (s : ss) =
      let !s' = Magn.update c s (clip b d)
          !rest = step_magn bt ss
      in  s' : rest
    step_magn _ _ = []
    step_cdf (x : xs) (s : ss) =
      let !s' = Magn.update cfg_cdf s (clip cfg_cdf_bound x)
          !rest = step_cdf xs ss
      in  s' : rest
    step_cdf _ _ = []

-- | The hedge after one more pair: its difference, and at each cut
--   point whether its first value lies at or below it, less whether
--   its second does (the indicators' bound being one).
update_pair :: Config -> State -> (Double, Double) -> State
update_pair cfg st (!a, !b) =
  update cfg st (a - b) [ below a q - below b q | q <- cfg_cuts cfg ]
  where
    below x q = if x <= q then 1 else 0
{-# INLINE update_pair #-}

clip :: Double -> Double -> Double
clip !b !x
  | x > b = b
  | x < negate b = negate b
  | otherwise = x
{-# INLINE clip #-}

-- | 'Reject' iff the hedge's log e-value has reached @log(1 \/ alpha)@
--   at some observation so far.
decide :: Config -> State -> Verdict
decide Config{..} State{..}
  | st_sup >= cfg_log_thresh = Reject
  | otherwise = Continue
{-# INLINE decide #-}

-- inspection ----------------------------------------------------------------

-- | The hedge's current log e-value: the log of its components' mean,
--   of the clipped components' alone under an interval null.
log_evalue :: Config -> State -> Double
log_evalue Config{..} State{..}
  | cfg_interval = log_mean (fmap Magn.log_evalue st_magn)
  | otherwise = log_mean
      (Sign.log_evalue st_sign
       : fmap Magn.log_evalue st_magn <> fmap Magn.log_evalue st_cdf)
{-# INLINE log_evalue #-}

-- | The supremum so far of the hedge's log e-value; zero before any
--   observation.
log_evalue_sup :: State -> Double
log_evalue_sup = st_sup
{-# INLINE log_evalue_sup #-}

-- | The sign component's current log e-value.
log_evalue_sign :: State -> Double
log_evalue_sign = Sign.log_evalue . st_sign
{-# INLINE log_evalue_sign #-}

-- | Each clipped component's current log e-value, in the order of the
--   clip bounds.
log_evalue_magnitude :: State -> [Double]
log_evalue_magnitude = fmap Magn.log_evalue . st_magn
{-# INLINE log_evalue_magnitude #-}

-- | Each indicator component's current log e-value, in the order of
--   the cut points.
log_evalue_cdf :: State -> [Double]
log_evalue_cdf = fmap Magn.log_evalue . st_cdf
{-# INLINE log_evalue_cdf #-}

-- | How many observations the hedge has seen.
samples :: State -> Int
samples = st_n
{-# INLINE samples #-}
