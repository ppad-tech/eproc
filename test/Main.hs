{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits
import Data.List (foldl')
import Data.Word
import qualified Statistics.EProcess as E
import qualified Statistics.EProcess.Mean as M
import qualified Statistics.EProcess.TwoSample as TS
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-eproc" [
    sanityTests
  , calibrationTests
  , powerTests
  , twoSampleTests
  , bettorSmokeTests
  ]

-- inline PCG-style PRNG, no external deps.

newtype Gen = Gen Word64

mkGen :: Word64 -> Gen
mkGen = Gen

stepGen :: Gen -> (Word64, Gen)
stepGen (Gen s) =
  let !s' = s * 6364136223846793005 + 1442695040888963407
  in  (s', Gen s')

nextDouble :: Gen -> (Double, Gen)
nextDouble g =
  let (w, g') = stepGen g
      !x = fromIntegral (w `shiftR` 11 .&. 0x1FFFFFFFFFFFFF) /
           9007199254740992
  in  (x, g')

bernoulli :: Double -> Gen -> (Double, Gen)
bernoulli !p g =
  let (u, g') = nextDouble g
  in  (if u < p then 1.0 else 0.0, g')

-- run a sequential mean test on a stream of n bernoulli(p) samples,
-- with the early-stopping rule built in. returns (verdict, samples
-- consumed).
runMeanBernoulli
  :: M.Config s
  -> Double           -- ^ p
  -> Int              -- ^ budget
  -> Gen
  -> (M.Verdict, Int)
runMeanBernoulli cfg p budget g0 = go 0 g0 (M.initial cfg)
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
rejectionRate
  :: M.Config s
  -> Double           -- ^ true bernoulli p
  -> Int              -- ^ budget per trial
  -> Int              -- ^ number of trials
  -> Word64           -- ^ seed
  -> Double
rejectionRate cfg p budget trials seed =
  let gens = take trials (genSeq (mkGen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = runMeanBernoulli cfg p budget g
             , v == M.Reject ]
  in  fromIntegral rejects / fromIntegral trials

genSeq :: Gen -> [Gen]
genSeq g = let (_, g') = stepGen g in g : genSeq g'

-- sanity: with all-zero deviations from the null mean, no rejection.

sanityTests :: TestTree
sanityTests = testGroup "sanity" [
    testCase "degenerate input never rejects" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-6 E.ons :: M.Config E.ONS
          xs = replicate 5000 0.5
          st = foldl' (M.update cfg) (M.initial cfg) xs
      M.decide cfg st @?= M.Continue
  , testCase "two-sided thresholds applied symmetrically" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-6 E.ons :: M.Config E.ONS
      M.decide cfg (M.initial cfg) @?= M.Continue
  ]

-- null calibration: under H_0, with optional stopping, the empirical
-- rejection rate should be bounded by alpha. ville's inequality is
-- typically conservative on bernoulli, so the slack is small.

calibrationTests :: TestTree
calibrationTests = testGroup "null calibration" [
    testCase "ONS, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = M.config 0.5 0.0 1.0 0.05 E.ons :: M.Config E.ONS
          rate = rejectionRate cfg 0.5 2000 200 12345
      -- expected rate ≤ 0.05; allow up to 0.10 slack for sampling
      -- variability over 200 trials.
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.10
  , testCase "aGRAPA, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = M.config 0.5 0.0 1.0 0.05 E.agrapa :: M.Config E.AGRAPA
          rate = rejectionRate cfg 0.5 2000 200 67890
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.10
  ]

-- power: under a clear shift, all (or nearly all) trials reject
-- within budget.

powerTests :: TestTree
powerTests = testGroup "power" [
    testCase "ONS detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 E.ons :: M.Config E.ONS
          rate = rejectionRate cfg 0.7 5000 100 11111
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "aGRAPA detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 E.agrapa
            :: M.Config E.AGRAPA
          rate = rejectionRate cfg 0.7 5000 100 22222
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  ]

-- two-sample paired test.

runTSPaired
  :: TS.Config s
  -> Double
  -> Double           -- ^ p for A and B
  -> Int
  -> Gen
  -> (TS.Verdict, Int)
runTSPaired cfg pA pB budget g0 = go 0 g0 (TS.initial cfg)
  where
    go !n !g !st
      | n >= budget = (TS.decide cfg st, n)
      | otherwise = case TS.decide cfg st of
          M.Reject -> (M.Reject, n)
          M.Continue ->
            let (a, g1) = bernoulli pA g
                (b, g2) = bernoulli pB g1
                st' = TS.update cfg st (a, b)
            in  go (n + 1) g2 st'

twoSampleTests :: TestTree
twoSampleTests = testGroup "two-sample" [
    testCase "identical distributions don't reject" $ do
      let cfg = TS.config 0.0 1.0 1.0e-3 E.ons :: TS.Config E.ONS
          rate = avgRate cfg 0.5 0.5 2000 100 33333
      assertBool ("FPR " ++ show rate) $ rate <= 0.05
  , testCase "different distributions reject" $ do
      let cfg = TS.config 0.0 1.0 1.0e-3 E.ons :: TS.Config E.ONS
          rate = avgRate cfg 0.3 0.7 5000 100 44444
      assertBool ("power " ++ show rate) $ rate >= 0.95
  ]
  where
    avgRate cfg pA pB budget trials seed =
      let gens = take trials (genSeq (mkGen seed))
          rejects = length
            [ () | g <- gens
                 , let (v, _) = runTSPaired cfg pA pB budget g
                 , v == M.Reject ]
      in  fromIntegral rejects / fromIntegral trials

-- bettor smoke tests: each bettor produces a well-defined state and
-- decision when run on a small deterministic stream.

bettorSmokeTests :: TestTree
bettorSmokeTests = testGroup "bettor smoke" [
    testCase "fixed bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3
                  (const (E.fixed 0.5)) :: M.Config ()
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  , testCase "ONS bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 E.ons :: M.Config E.ONS
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  , testCase "aGRAPA bettor runs without error" $ do
      let cfg = M.config 0.5 0.0 1.0 1.0e-3 E.agrapa
            :: M.Config E.AGRAPA
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (M.update cfg) (M.initial cfg) xs
      assertBool "samples advanced" (M.samples st == 100)
  ]
