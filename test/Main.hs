{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits
import Data.Word
import qualified Numeric.Eproc.Bettor as B
import qualified Numeric.Eproc.Mean as M
import qualified Numeric.Eproc.Test as T
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-eproc" [
    sanity_tests
  , calibration_tests
  , power_tests
  , two_sample_tests
  , bettor_smoke_tests
  ]

-- prng -----------------------------------------------------------------------

-- inline PCG-style PRNG, no external deps.

newtype Gen = Gen Word64

mk_gen :: Word64 -> Gen
mk_gen = Gen

step_gen :: Gen -> (Word64, Gen)
step_gen (Gen s) =
  let !s' = s * 6364136223846793005 + 1442695040888963407
  in  (s', Gen s')

next_double :: Gen -> (Double, Gen)
next_double g =
  let (w, g') = step_gen g
      !x = fromIntegral (w `shiftR` 11 .&. 0x1FFFFFFFFFFFFF) /
           9007199254740992
  in  (x, g')

bernoulli :: Double -> Gen -> (Double, Gen)
bernoulli !p g =
  let (u, g') = next_double g
  in  (if u < p then 1.0 else 0.0, g')

gen_seq :: Gen -> [Gen]
gen_seq g = let (_, g') = step_gen g in g : gen_seq g'

-- harness --------------------------------------------------------------------

-- run a sequential mean test on a stream of n bernoulli(p) samples,
-- with the early-stopping rule built in. returns (verdict, samples
-- consumed).
run_mean_bernoulli
  :: M.Config
  -> Double           -- ^ p
  -> Int              -- ^ budget
  -> Gen
  -> (M.Verdict, Int)
run_mean_bernoulli cfg p budget g0 = go 0 g0 (M.initial cfg)
  where
    go !n !g !st
      | n >= budget = (M.decide cfg st, n)
      | otherwise = case M.decide cfg st of
          M.Reject -> (M.Reject, n)
          M.Continue ->
            let (x, g') = bernoulli p g
                st' = M.update cfg st x
            in  go (n + 1) g' st'

-- fraction of trials that rejected.
rejection_rate
  :: M.Config
  -> Double           -- ^ true bernoulli p
  -> Int              -- ^ budget per trial
  -> Int              -- ^ number of trials
  -> Word64           -- ^ seed
  -> Double
rejection_rate cfg p budget trials seed =
  let gens = take trials (gen_seq (mk_gen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = run_mean_bernoulli cfg p budget g
             , v == M.Reject ]
  in  fromIntegral rejects / fromIntegral trials

run_paired
  :: T.Config
  -> Double
  -> Double           -- ^ p for A and B
  -> Int
  -> Gen
  -> (T.Verdict, Int)
run_paired cfg pa pb budget g0 = go 0 g0 (T.initial cfg)
  where
    go !n !g !st
      | n >= budget = (T.decide cfg st, n)
      | otherwise = case T.decide cfg st of
          M.Reject -> (M.Reject, n)
          M.Continue ->
            let (a, g1) = bernoulli pa g
                (b, g2) = bernoulli pb g1
                st' = T.update cfg st (a, b)
            in  go (n + 1) g2 st'

paired_avg_rate
  :: T.Config
  -> Double
  -> Double
  -> Int
  -> Int
  -> Word64
  -> Double
paired_avg_rate cfg pa pb budget trials seed =
  let gens = take trials (gen_seq (mk_gen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = run_paired cfg pa pb budget g
             , v == M.Reject ]
  in  fromIntegral rejects / fromIntegral trials

-- sanity ---------------------------------------------------------------------

-- with all-zero deviations from the null mean, no rejection.
sanity_tests :: TestTree
sanity_tests = testGroup "sanity" [
    testCase "degenerate input never rejects" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-6 B.Ons
          xs = replicate 5000 0.5
          st = foldl' (M.update cfg) (M.initial cfg) xs
      M.decide cfg st @?= M.Continue
  , testCase "two-sided thresholds applied symmetrically" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-6 B.Ons
      M.decide cfg (M.initial cfg) @?= M.Continue
  ]

-- null calibration -----------------------------------------------------------

-- under H_0, with optional stopping, the empirical rejection rate should be
-- bounded by alpha. ville's inequality is typically conservative on bernoulli,
-- so the slack is small.
calibration_tests :: TestTree
calibration_tests = testGroup "null calibration" [
    testCase "ONS, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = M.config 0.5 0.0 1.0 0.05 B.Ons
          rate = rejection_rate cfg 0.5 2000 200 12345
      -- expected rate <= 0.05; allow up to 0.10 slack for sampling
      -- variability over 200 trials.
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.10
  , testCase "aGRAPA, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = M.config 0.5 0.0 1.0 0.05 B.Agrapa
          rate = rejection_rate cfg 0.5 2000 200 67890
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.10
  ]

-- power ----------------------------------------------------------------------

-- under a clear shift, all (or nearly all) trials reject within budget.
power_tests :: TestTree
power_tests = testGroup "power" [
    testCase "ONS detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
          rate = rejection_rate cfg 0.7 5000 100 11111
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "aGRAPA detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
          rate = rejection_rate cfg 0.7 5000 100 22222
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  ]

-- two-sample paired test -----------------------------------------------------

two_sample_tests :: TestTree
two_sample_tests = testGroup "two-sample" [
    testCase "identical distributions don't reject" $ do
      let cfg = T.config 0.0 1.0 1.0e-3 B.Ons
          rate = paired_avg_rate cfg 0.5 0.5 2000 100 33333
      assertBool ("FPR " ++ show rate) $ rate <= 0.05
  , testCase "different distributions reject" $ do
      let cfg = T.config 0.0 1.0 1.0e-3 B.Ons
          rate = paired_avg_rate cfg 0.3 0.7 5000 100 44444
      assertBool ("power " ++ show rate) $ rate >= 0.95
  ]

-- bettor smoke tests ---------------------------------------------------------

-- each bettor produces a well-defined state and decision when run on a small
-- deterministic stream.
bettor_smoke_tests :: TestTree
bettor_smoke_tests = testGroup "bettor smoke" [
    testCase "fixed bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  , testCase "ONS bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  , testCase "aGRAPA bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  ]
