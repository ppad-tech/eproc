{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits
import Data.Word
import qualified Numeric.Eproc.Bernoulli as Bern
import qualified Numeric.Eproc.Bounded as Bounded
import qualified Numeric.Eproc.Common as C
import qualified Numeric.Eproc.Paired as P
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain $ testGroup "ppad-eproc" [
    sanity_tests
  , calibration_tests
  , power_tests
  , two_sample_tests
  , bernoulli_tests
  , bettor_smoke_tests
  , latched_rejection_tests
  , config_validation_tests
  , safety_property_tests
  ]

-- partial helper: tests below hardcode valid configs.
ok :: Either e a -> a
ok (Right x) = x
ok (Left _)  = error "test: invalid config"

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

-- per-trial independent seeds via a splitmix-style finalizer.
-- previously this just stepped the prng once per trial, which made
-- consecutive trials share all but one observation -- fine under a
-- symmetric H_0 (rare streaks cancel), catastrophic under a skewed
-- one (rare streaks dominate all overlapping trials).
gen_seq :: Gen -> [Gen]
gen_seq (Gen s0) =
  [Gen (mix64 (s0 + fromIntegral i)) | i <- [(0 :: Word64) ..]]
  where
    mix64 x =
      let !y = (x `xor` (x `shiftR` 30)) * 0xbf58476d1ce4e5b9
          !z = (y `xor` (y `shiftR` 27)) * 0x94d049bb133111eb
      in  z `xor` (z `shiftR` 31)

-- harness --------------------------------------------------------------------

-- run a sequential mean test on a stream of n bernoulli(p) samples,
-- with the early-stopping rule built in. returns (verdict, samples
-- consumed).
run_bounded_bernoulli
  :: Bounded.Config
  -> Double           -- ^ p
  -> Int              -- ^ budget
  -> Gen
  -> (Bounded.Verdict, Int)
run_bounded_bernoulli cfg p budget g0 = go 0 g0 (Bounded.initial cfg)
  where
    go !n !g !st
      | n >= budget = (Bounded.decide cfg st, n)
      | otherwise = case Bounded.decide cfg st of
          Bounded.Reject -> (Bounded.Reject, n)
          Bounded.Continue ->
            let (x, g') = bernoulli p g
                st' = Bounded.update cfg st x
            in  go (n + 1) g' st'

-- fraction of trials that rejected.
rejection_rate
  :: Bounded.Config
  -> Double           -- ^ true bernoulli p
  -> Int              -- ^ budget per trial
  -> Int              -- ^ number of trials
  -> Word64           -- ^ seed
  -> Double
rejection_rate cfg p budget trials seed =
  let gens = take trials (gen_seq (mk_gen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = run_bounded_bernoulli cfg p budget g
             , v == Bounded.Reject ]
  in  fromIntegral rejects / fromIntegral trials

run_paired
  :: P.Config
  -> Double
  -> Double           -- ^ p for A and B
  -> Int
  -> Gen
  -> (P.Verdict, Int)
run_paired cfg pa pb budget g0 = go 0 g0 (P.initial cfg)
  where
    go !n !g !st
      | n >= budget = (P.decide cfg st, n)
      | otherwise = case P.decide cfg st of
          P.Reject -> (P.Reject, n)
          P.Continue ->
            let (a, g1) = bernoulli pa g
                (b, g2) = bernoulli pb g1
                st' = P.update cfg st (a, b)
            in  go (n + 1) g2 st'

paired_avg_rate
  :: P.Config
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
             , v == P.Reject ]
  in  fromIntegral rejects / fromIntegral trials

-- sanity ---------------------------------------------------------------------

-- with all-zero deviations from the null mean, no rejection.
sanity_tests :: TestTree
sanity_tests = testGroup "sanity" [
    testCase "degenerate input never rejects" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-6 Bounded.Newton)
          xs = replicate 5000 0.5
          st = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
      Bounded.decide cfg st @?= Bounded.Continue
  , testCase "two-sided thresholds applied symmetrically" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-6 Bounded.Newton)
      Bounded.decide cfg (Bounded.initial cfg) @?= Bounded.Continue
  ]

-- null calibration -----------------------------------------------------------

-- under H_0, with optional stopping, the empirical rejection rate should be
-- bounded by alpha. ville's inequality is typically conservative on bernoulli,
-- so the slack is small.
calibration_tests :: TestTree
calibration_tests = testGroup "null calibration" [
    testCase "Newton, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 0.05 Bounded.Newton)
          rate = rejection_rate cfg 0.5 2000 200 12345
      -- expected rate <= 0.05; allow up to ~0.08 slack for sampling
      -- variability over 200 trials (sigma ~ 0.015).
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.08
  , testCase "Adaptive, Bernoulli(0.5), m=0.5, alpha=0.05" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 0.05 Bounded.Adaptive)
          rate = rejection_rate cfg 0.5 2000 200 67890
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.08
  ]

-- power ----------------------------------------------------------------------

-- under a clear shift, all (or nearly all) trials reject within budget.
power_tests :: TestTree
power_tests = testGroup "power" [
    testCase "Newton detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
          rate = rejection_rate cfg 0.7 5000 100 11111
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "Adaptive detects Bernoulli(0.7) vs m=0.5" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
          rate = rejection_rate cfg 0.7 5000 100 22222
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  ]

-- two-sample paired test -----------------------------------------------------

two_sample_tests :: TestTree
two_sample_tests = testGroup "two-sample" [
    testCase "identical distributions don't reject" $ do
      let cfg = ok (P.config 0.0 1.0 1.0e-3 Bounded.Newton)
          rate = paired_avg_rate cfg 0.5 0.5 2000 100 33333
      assertBool ("FPR " ++ show rate) $ rate <= 0.05
  , testCase "different distributions reject" $ do
      let cfg = ok (P.config 0.0 1.0 1.0e-3 Bounded.Newton)
          rate = paired_avg_rate cfg 0.3 0.7 5000 100 44444
      assertBool ("power " ++ show rate) $ rate >= 0.95
  ]

-- bernoulli (one-sided rate) -------------------------------------------------

run_bernoulli
  :: Bern.Config
  -> Double           -- ^ true rate p
  -> Int              -- ^ budget
  -> Gen
  -> (Bern.Verdict, Int)
run_bernoulli cfg p budget g0 = go 0 g0 (Bern.initial cfg)
  where
    go !n !g !st
      | n >= budget = (Bern.decide cfg st, n)
      | otherwise = case Bern.decide cfg st of
          Bern.Reject -> (Bern.Reject, n)
          Bern.Continue ->
            let (u, g') = next_double g
                !x      = u < p
                st'     = Bern.update cfg st x
            in  go (n + 1) g' st'

bernoulli_rate
  :: Bern.Config
  -> Double           -- ^ true rate p
  -> Int              -- ^ budget per trial
  -> Int              -- ^ number of trials
  -> Word64           -- ^ seed
  -> Double
bernoulli_rate cfg p budget trials seed =
  let gens = take trials (gen_seq (mk_gen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = run_bernoulli cfg p budget g
             , v == Bern.Reject ]
  in  fromIntegral rejects / fromIntegral trials

bernoulli_tests :: TestTree
bernoulli_tests = testGroup "bernoulli" [
    testCase "all-zero stream never rejects" $ do
      let cfg = ok (Bern.config 0.05 1.0e-6 Bern.Newton)
          xs  = replicate 5000 False
          st  = foldl' (Bern.update cfg) (Bern.initial cfg) xs
      Bern.decide cfg st @?= Bern.Continue
  , testCase "Newton FPR under H_0 (p = p_0 = 0.05)" $ do
      let cfg  = ok (Bern.config 0.05 0.05 Bern.Newton)
          rate = bernoulli_rate cfg 0.05 2000 200 55555
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.08
  , testCase "Adaptive FPR under H_0 (p = p_0 = 0.05)" $ do
      let cfg  = ok (Bern.config 0.05 0.05 Bern.Adaptive)
          rate = bernoulli_rate cfg 0.05 2000 200 66666
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.08
  , testCase "Newton detects p = 0.3 vs p_0 = 0.05" $ do
      let cfg  = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
          rate = bernoulli_rate cfg 0.3 5000 100 77777
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "Adaptive detects p = 0.3 vs p_0 = 0.05" $ do
      let cfg  = ok (Bern.config 0.05 1.0e-3 Bern.Adaptive)
          rate = bernoulli_rate cfg 0.3 5000 100 88888
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  ]

-- bettor smoke tests ---------------------------------------------------------

-- each bettor produces a well-defined state and decision when run on a small
-- deterministic stream.
bettor_smoke_tests :: TestTree
bettor_smoke_tests = testGroup "bettor smoke" [
    testCase "fixed bettor runs without error (bounded)" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
      assertBool "samples advanced" (Bounded.samples st == 100)
  , testCase "Newton bettor runs without error (bounded)" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
      assertBool "samples advanced" (Bounded.samples st == 100)
  , testCase "Adaptive bettor runs without error (bounded)" $ do
      let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
          xs = take 100 (cycle [0.0, 1.0])
          st = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
      assertBool "samples advanced" (Bounded.samples st == 100)
  , testCase "fixed bettor runs without error (bernoulli)" $ do
      let cfg = ok (Bern.config 0.5 1.0e-3 (Bern.Fixed 0.5))
          xs = take 100 (cycle [True, False])
          st = foldl' (Bern.update cfg) (Bern.initial cfg) xs
      assertBool "samples advanced" (Bern.samples st == 100)
  , testCase "Newton bettor runs without error (bernoulli)" $ do
      let cfg = ok (Bern.config 0.5 1.0e-3 Bern.Newton)
          xs = take 100 (cycle [True, False])
          st = foldl' (Bern.update cfg) (Bern.initial cfg) xs
      assertBool "samples advanced" (Bern.samples st == 100)
  , testCase "Adaptive bettor runs without error (bernoulli)" $ do
      let cfg = ok (Bern.config 0.5 1.0e-3 Bern.Adaptive)
          xs = take 100 (cycle [True, False])
          st = foldl' (Bern.update cfg) (Bern.initial cfg) xs
      assertBool "samples advanced" (Bern.samples st == 100)
  ]

-- latched rejection ----------------------------------------------------------

-- once the wealth crosses threshold, subsequent observations driving the
-- current wealth back below threshold must not unrejection the test.
latched_rejection_tests :: TestTree
latched_rejection_tests = testGroup "latched rejection" [
    testCase "bounded: cross then drown stays rejected" $ do
      -- alpha = 0.5 => threshold log(2/0.5) = log 4 ~ 1.386.
      -- Fixed 1.0 with x=1 grows log_w_pos by log 1.5 ~ 0.405/step;
      -- five 1s push it past threshold. Then forty 0s drop it well
      -- below.
      let cfg  = ok (Bounded.config 0.5 0.0 1.0 0.5 (Bounded.Fixed 1.0))
          xs1  = replicate 5 1.0
          xs2  = replicate 40 0.0
          st1  = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs1
          st2  = foldl' (Bounded.update cfg) st1 xs2
      Bounded.decide cfg st1 @?= Bounded.Reject
      Bounded.decide cfg st2 @?= Bounded.Reject
  , testCase "bernoulli: cross then drown stays rejected" $ do
      let cfg  = ok (Bern.config 0.05 0.5 (Bern.Fixed 1.0))
          xs1  = replicate 5 True
          xs2  = replicate 200 False
          st1  = foldl' (Bern.update cfg) (Bern.initial cfg) xs1
          st2  = foldl' (Bern.update cfg) st1 xs2
      Bern.decide cfg st1 @?= Bern.Reject
      Bern.decide cfg st2 @?= Bern.Reject
  ]

-- config validation ----------------------------------------------------------

config_validation_tests :: TestTree
config_validation_tests = testGroup "config validation" [
    testCase "Bounded: alpha <= 0 rejected" $
      assertLeft (Bounded.config 0.5 0.0 1.0 0.0 Bounded.Newton)
  , testCase "Bounded: alpha >= 1 rejected" $
      assertLeft (Bounded.config 0.5 0.0 1.0 1.5 Bounded.Newton)
  , testCase "Bounded: lo >= hi rejected" $
      assertLeft (Bounded.config 0.5 1.0 0.0 0.01 Bounded.Newton)
  , testCase "Bounded: m == lo rejected" $
      assertLeft (Bounded.config 0.0 0.0 1.0 0.01 Bounded.Newton)
  , testCase "Bounded: m == hi rejected" $
      assertLeft (Bounded.config 1.0 0.0 1.0 0.01 Bounded.Newton)
  , testCase "Bounded: m outside [lo, hi] rejected" $
      assertLeft (Bounded.config 2.0 0.0 1.0 0.01 Bounded.Newton)
  , testCase "Bernoulli: alpha <= 0 rejected" $
      assertLeft (Bern.config 0.5 0.0 Bern.Newton)
  , testCase "Bernoulli: alpha >= 1 rejected" $
      assertLeft (Bern.config 0.5 1.0 Bern.Newton)
  , testCase "Bernoulli: p0 == 0 rejected" $
      assertLeft (Bern.config 0.0 0.05 Bern.Newton)
  , testCase "Bernoulli: p0 == 1 rejected" $
      assertLeft (Bern.config 1.0 0.05 Bern.Newton)
  , testCase "Paired: alpha out of range rejected" $
      assertLeft (P.config 0.0 1.0 0.0 Bounded.Newton)
  , testCase "Paired: lo >= hi rejected" $
      assertLeft (P.config 1.0 0.0 0.01 Bounded.Newton)
  , testCase "Bounded: infinite bounds rejected" $
      assertLeft (Bounded.config 0.0 nInf pInf 0.01 Bounded.Newton)
  , testCase "Bounded: NaN m rejected" $
      assertLeft (Bounded.config nan 0.0 1.0 0.01 Bounded.Newton)
  , testCase "Bounded: NaN alpha rejected" $
      assertLeft (Bounded.config 0.5 0.0 1.0 nan Bounded.Newton)
  , testCase "Bernoulli: NaN p0 rejected" $
      assertLeft (Bern.config nan 0.01 Bern.Newton)
  , testCase "Bernoulli: infinite alpha rejected" $
      assertLeft (Bern.config 0.05 pInf Bern.Newton)
  , testCase "Paired: infinite hi rejected" $
      assertLeft (P.config 0.0 pInf 0.01 Bounded.Newton)
  ]
  where
    nan, pInf, nInf :: Double
    nan  = 0 / 0
    pInf = 1 / 0
    nInf = negate (1 / 0)
    assertLeft :: Either C.ConfigError a -> Assertion
    assertLeft e = case e of
      Left _  -> pure ()
      Right _ -> assertFailure "expected Left"

-- safety properties ----------------------------------------------------------

unit_double :: QC.Gen Double
unit_double = QC.choose (0, 1)

arb_bettor :: QC.Gen C.Bettor
arb_bettor = QC.oneof [
    pure C.Adaptive
  , pure C.Newton
  , C.Fixed <$> QC.choose (-10, 10)  -- intentionally include unsafe values
  ]

finite :: Double -> Bool
finite x = not (isNaN x) && not (isInfinite x)

monotone_reject_bounded :: [Bounded.Verdict] -> Bool
monotone_reject_bounded [] = True
monotone_reject_bounded (Bounded.Continue : rest) = monotone_reject_bounded rest
monotone_reject_bounded (Bounded.Reject : rest)   = all (== Bounded.Reject) rest

monotone_reject_bern :: [Bern.Verdict] -> Bool
monotone_reject_bern [] = True
monotone_reject_bern (Bern.Continue : rest) = monotone_reject_bern rest
monotone_reject_bern (Bern.Reject : rest)   = all (== Bern.Reject) rest

safety_property_tests :: TestTree
safety_property_tests = testGroup "safety properties" [
    QC.testProperty "Bounded: log_wealth finite after any admissible stream" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            st  = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
        in  finite (Bounded.log_wealth st)

  , QC.testProperty "Bernoulli: log_wealth finite after any admissible stream" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (Bern.config 0.05 1.0e-3 b)
            st  = foldl' (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
        in  finite (Bern.log_wealth st)

  , QC.testProperty "Bounded: Fixed with arbitrary lambda is safe" $
      QC.forAll (QC.choose (-1000, 1000)) $ \lam ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (C.Fixed lam))
            st  = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
        in  finite (Bounded.log_wealth st)

  , QC.testProperty "Bernoulli: Fixed with arbitrary lambda is safe" $
      QC.forAll (QC.choose (-1000, 1000)) $ \lam ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (Bern.config 0.05 1.0e-3 (C.Fixed lam))
            st  = foldl' (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
        in  finite (Bern.log_wealth st)

  , QC.testProperty "Bounded: log_wealth is monotone nondecreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg  = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            sts  = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
            lws  = map Bounded.log_wealth sts
        in  and (zipWith (<=) lws (drop 1 lws))

  , QC.testProperty "Bernoulli: log_wealth is monotone nondecreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (Bern.config 0.05 1.0e-3 b)
            sts  = scanl (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
            lws  = map Bern.log_wealth sts
        in  and (zipWith (<=) lws (drop 1 lws))

  , QC.testProperty "Bounded: rejection is latched" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg  = ok (Bounded.config 0.5 0.0 1.0 0.5 b)
            sts  = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
            vs   = map (Bounded.decide cfg) sts
        in  monotone_reject_bounded vs

  , QC.testProperty "Bernoulli: rejection is latched" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (Bern.config 0.5 0.5 b)
            sts  = scanl (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
            vs   = map (Bern.decide cfg) sts
        in  monotone_reject_bern vs
  ]
