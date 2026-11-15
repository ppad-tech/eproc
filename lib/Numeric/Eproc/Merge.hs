{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Numeric.Eproc.Merge
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Merging e-values, and choosing among the hypotheses they test.
--
-- An e-value for a null @H_0@ is a nonnegative statistic @E@ with
-- @E[E] <= 1@ under @H_0@. Two ways of merging them keep that
-- property:
--
--   * The mean of any e-values for one null is an e-value for it,
--     however they depend on one another ('log_mean'): the merge a
--     test over several observables makes, paying a factor of their
--     number.
--
--   * The product of e-values each valid given those before it (for
--     instance, computed on independent data) is an e-value
--     ('log_product'): the merge of a test continued on fresh data.
--
-- An e-process evaluated at a stopping time is an e-value, so both
-- apply to e-processes stopped when the caller likes.
--
-- Given e-values for several nulls, one each, 'ebh' chooses which to
-- reject: the e-BH procedure of Wang & Ramdas (2022), whose false
-- discovery rate is at most its level under any dependence between
-- the e-values.
--
-- Every function here takes and returns e-values as their natural
-- logarithms, as the test modules' @log_evalue@ accessors report
-- them.

module Numeric.Eproc.Merge (
    log_mean
  , log_product
  , ebh
  ) where

import qualified Data.List as L
import Data.Ord (Down(..), comparing)

-- | The log of the mean of e-values given as logs: an e-value for
--   the null they share, whatever their dependence. The mean of no
--   e-values is taken to be one.
--
--   >>> round (exp (log_mean [log 2, log 4]) * 1000)
--   3000
--   >>> log_mean []
--   0.0
--   >>> log_mean [1 / 0, 0]
--   Infinity
log_mean :: [Double] -> Double
log_mean les = case les of
  [] -> 0
  (l : ls) ->
    let !m = L.foldl' max l ls
        !s = L.foldl' (\ !acc v -> acc + exp (v - m)) 0 les
        !k = fromIntegral (length les) :: Double
    -- an infinite e-value, of either sign, is the mean's sign too: an
    -- e-value of zero among no greater, or one infinitely great
    in  if isInfinite m
          then m
          else m + log s - log k
{-# INLINE log_mean #-}

-- | The log of the product of e-values given as logs: an e-value
--   where each is valid given those before it.
--
--   >>> log_product [log 2, log 3] == log 6
--   True
log_product :: [Double] -> Double
log_product = L.foldl' (+) 0
{-# INLINE log_product #-}

-- | The e-BH discoveries at a level among e-values, one per null,
--   given as logs: the indices of the @k@ largest, for the largest
--   @k@ whose @k@-th largest e-value is at least @n \/ (alpha k)@, for
--   @n@ of them, in decreasing order of e-value. The expected share
--   of discoveries whose null holds is at most @alpha@, under any
--   dependence between the e-values.
--
--   >>> ebh 0.1 [log 100, 0, log 14]
--   [0]
--   >>> ebh 0.1 [log 100, 0, log 15]
--   [0,2]
ebh :: Double -> [Double] -> [Int]
ebh !alpha les = take k ranked
  where
    !n = fromIntegral (length les) :: Double
    ranked = fmap fst (L.sortBy (comparing (Down . snd)) (zip [0 ..] les))
    sorted = L.sortBy (comparing Down) les
    !k = L.foldl' max 0
      [ j
      | (j, e) <- zip [ 1 .. ] sorted
      , e >= log (n / (alpha * fromIntegral j)) ]
