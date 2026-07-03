{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Bits
import Data.Word
import qualified Numeric.Eproc.Bernoulli as Bern
import qualified Numeric.Eproc.Bernoulli.TwoSided as BernTS
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
  , two_sided_bernoulli_tests
  , evalue_accessor_tests
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

-- two-sided bernoulli --------------------------------------------------------

run_ts_bernoulli
  :: BernTS.Config
  -> Double           -- ^ true rate p
  -> Int              -- ^ budget
  -> Gen
  -> (BernTS.Verdict, Int)
run_ts_bernoulli cfg p budget g0 =
  go 0 g0 (BernTS.initial cfg)
  where
    go !n !g !st
      | n >= budget = (BernTS.decide cfg st, n)
      | otherwise = case BernTS.decide cfg st of
          BernTS.Reject -> (BernTS.Reject, n)
          BernTS.Continue ->
            let (u, g') = next_double g
                !x      = u < p
                st'     = BernTS.update cfg st x
            in  go (n + 1) g' st'

ts_bernoulli_rate
  :: BernTS.Config
  -> Double
  -> Int
  -> Int
  -> Word64
  -> Double
ts_bernoulli_rate cfg p budget trials seed =
  let gens = take trials (gen_seq (mk_gen seed))
      rejects = length
        [ () | g <- gens
             , let (v, _) = run_ts_bernoulli cfg p budget g
             , v == BernTS.Reject ]
  in  fromIntegral rejects / fromIntegral trials

two_sided_bernoulli_tests :: TestTree
two_sided_bernoulli_tests = testGroup "two-sided bernoulli" [
    testCase "constant at p_0 doesn't reject" $ do
      -- Bernoulli(0.5) with p_0 = 0.5 is under the null.
      let cfg = ok (BernTS.config 0.5 1.0e-6 BernTS.Newton)
          -- alternating True/False keeps the empirical rate at 0.5.
          xs  = take 5000 (cycle [True, False])
          st  = foldl' (BernTS.update cfg) (BernTS.initial cfg) xs
      BernTS.decide cfg st @?= BernTS.Continue
  , testCase "detects upward shift (p = 0.7 vs p_0 = 0.5)" $ do
      let cfg  = ok (BernTS.config 0.5 1.0e-3 BernTS.Newton)
          rate = ts_bernoulli_rate cfg 0.7 5000 100 111222
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "detects downward shift (p = 0.3 vs p_0 = 0.5)" $ do
      let cfg  = ok (BernTS.config 0.5 1.0e-3 BernTS.Newton)
          rate = ts_bernoulli_rate cfg 0.3 5000 100 333444
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "Adaptive detects shift (p = 0.7 vs p_0 = 0.5)" $ do
      let cfg  = ok (BernTS.config 0.5 1.0e-3 BernTS.Adaptive)
          rate = ts_bernoulli_rate cfg 0.7 5000 100 777888
      assertBool ("power " ++ show rate ++ " too low") $
        rate >= 0.95
  , testCase "FPR at p = p_0 = 0.5 within slack" $ do
      let cfg  = ok (BernTS.config 0.5 0.05 BernTS.Newton)
          rate = ts_bernoulli_rate cfg 0.5 2000 200 555666
      assertBool ("FPR " ++ show rate ++ " exceeded slack") $
        rate <= 0.08
  , testCase "latched: cross then drown stays rejected" $ do
      let cfg  = ok (BernTS.config 0.5 0.5 (BernTS.Fixed 1.0))
          -- ten 1s push the positive side well past threshold.
          xs1  = replicate 10 True
          -- then two hundred 0s drop the current wealth, but the
          -- latch must hold.
          xs2  = replicate 200 False
          st1  = foldl' (BernTS.update cfg) (BernTS.initial cfg) xs1
          st2  = foldl' (BernTS.update cfg) st1 xs2
      BernTS.decide cfg st1 @?= BernTS.Reject
      BernTS.decide cfg st2 @?= BernTS.Reject
  , testCase "config: NaN p0 rejected" $ do
      let nan = 0/0 :: Double
      case BernTS.config nan 0.05 BernTS.Newton of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left"
  , testCase "config: alpha out of range rejected" $
      case BernTS.config 0.5 1.5 BernTS.Newton of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left"
  ]

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

monotone_reject_bern_ts :: [BernTS.Verdict] -> Bool
monotone_reject_bern_ts [] = True
monotone_reject_bern_ts (BernTS.Continue : rest) = monotone_reject_bern_ts rest
monotone_reject_bern_ts (BernTS.Reject : rest)   = all (== BernTS.Reject) rest

safety_property_tests :: TestTree
safety_property_tests = testGroup "safety properties" [
    QC.testProperty "Bounded: log_wealth finite after any admissible stream" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            st  = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
        in  finite (Bounded.log_wealth st) &&
            finite (Bounded.log_wealth_sup st)

  , QC.testProperty "Bernoulli: log_wealth finite after any admissible stream" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (Bern.config 0.05 1.0e-3 b)
            st  = foldl' (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
        in  finite (Bern.log_wealth st) && finite (Bern.log_wealth_sup st)

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

  , QC.testProperty "Bounded: log_wealth_sup is monotone nondecreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg  = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            sts  = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
            lws  = map Bounded.log_wealth_sup sts
        in  and (zipWith (<=) lws (drop 1 lws))

  , QC.testProperty "Bernoulli: log_wealth_sup is monotone nondecreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (Bern.config 0.05 1.0e-3 b)
            sts  = scanl (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
            lws  = map Bern.log_wealth_sup sts
        in  and (zipWith (<=) lws (drop 1 lws))

  , QC.testProperty "Bounded: log_wealth bounded above by log_wealth_sup" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg  = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            sts  = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
        in  all (\s -> Bounded.log_wealth s <= Bounded.log_wealth_sup s) sts

  , QC.testProperty "Bernoulli: log_wealth bounded above by log_wealth_sup" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (Bern.config 0.05 1.0e-3 b)
            sts  = scanl (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
        in  all (\s -> Bern.log_wealth s <= Bern.log_wealth_sup s) sts

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

  , QC.testProperty "BernTS: log_wealth finite after any admissible stream" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (BernTS.config 0.5 1.0e-3 b)
            st  = foldl' (BernTS.update cfg) (BernTS.initial cfg) (xs :: [Bool])
        in  finite (BernTS.log_wealth st) && finite (BernTS.log_wealth_sup st)

  , QC.testProperty "BernTS: Fixed with arbitrary lambda is safe" $
      QC.forAll (QC.choose (-1000, 1000)) $ \lam ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (BernTS.config 0.5 1.0e-3 (C.Fixed lam))
            st  = foldl' (BernTS.update cfg) (BernTS.initial cfg) (xs :: [Bool])
        in  finite (BernTS.log_wealth st)

  , QC.testProperty "BernTS: log_wealth_sup is monotone nondecreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (BernTS.config 0.5 1.0e-3 b)
            sts  = scanl (BernTS.update cfg) (BernTS.initial cfg) (xs :: [Bool])
            lws  = map BernTS.log_wealth_sup sts
        in  and (zipWith (<=) lws (drop 1 lws))

  , QC.testProperty "BernTS: log_wealth bounded above by log_wealth_sup" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (BernTS.config 0.5 1.0e-3 b)
            sts  = scanl (BernTS.update cfg) (BernTS.initial cfg) (xs :: [Bool])
        in  all (\s -> BernTS.log_wealth s <= BernTS.log_wealth_sup s) sts

  , QC.testProperty "BernTS: rejection is latched" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg  = ok (BernTS.config 0.5 0.5 b)
            sts  = scanl (BernTS.update cfg) (BernTS.initial cfg) (xs :: [Bool])
            vs   = map (BernTS.decide cfg) sts
        in  monotone_reject_bern_ts vs
  ]

-- e-value accessors ----------------------------------------------------------

unit_pair :: QC.Gen (Double, Double)
unit_pair = (,) <$> unit_double <*> unit_double

evalue_accessor_tests :: TestTree
evalue_accessor_tests = testGroup "e-value accessors" [
    testCase "fresh states normalize to e-value 1, p-value 1" $ do
      let bcfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
          ncfg = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
          tcfg = ok (BernTS.config 0.5 1.0e-3 BernTS.Newton)
          pcfg = ok (P.config 0.0 1.0 1.0e-3 Bounded.Newton)
      Bounded.log_evalue (Bounded.initial bcfg) @?= 0.0
      Bounded.log_evalue_sup (Bounded.initial bcfg) @?= 0.0
      Bounded.p_value (Bounded.initial bcfg) @?= 1.0
      Bern.log_evalue (Bern.initial ncfg) @?= 0.0
      Bern.p_value (Bern.initial ncfg) @?= 1.0
      BernTS.log_evalue (BernTS.initial tcfg) @?= 0.0
      BernTS.p_value (BernTS.initial tcfg) @?= 1.0
      P.log_evalue (P.initial pcfg) @?= 0.0
      P.p_value (P.initial pcfg) @?= 1.0

  , QC.testProperty "Bounded: log_evalue is log_wealth less log 2" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            st  = foldl' (Bounded.update cfg) (Bounded.initial cfg) xs
        in  Bounded.log_evalue st == Bounded.log_wealth st - C.log2_dbl

  , QC.testProperty "Bernoulli: log_evalue coincides with log_wealth" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (Bern.config 0.05 1.0e-3 b)
            st  = foldl' (Bern.update cfg) (Bern.initial cfg) (xs :: [Bool])
        in  Bern.log_evalue st == Bern.log_wealth st

  , QC.testProperty "Bounded: decide agrees with p_value at alpha" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let alpha = 0.5
            cfg   = ok (Bounded.config 0.5 0.0 1.0 alpha b)
            sts   = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
        in  all (\s -> (Bounded.decide cfg s == Bounded.Reject)
                    == (Bounded.p_value s <= alpha)) sts

  , QC.testProperty "Bernoulli: decide agrees with p_value at alpha" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let alpha = 0.5
            cfg   = ok (Bern.config 0.5 alpha b)
            sts   = scanl (Bern.update cfg) (Bern.initial cfg)
                      (xs :: [Bool])
        in  all (\s -> (Bern.decide cfg s == Bern.Reject)
                    == (Bern.p_value s <= alpha)) sts

  , QC.testProperty "BernTS: decide agrees with p_value at alpha" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let alpha = 0.5
            cfg   = ok (BernTS.config 0.5 alpha b)
            sts   = scanl (BernTS.update cfg) (BernTS.initial cfg)
                      (xs :: [Bool])
        in  all (\s -> (BernTS.decide cfg s == BernTS.Reject)
                    == (BernTS.p_value s <= alpha)) sts

  , QC.testProperty "Bounded: p_value monotone nonincreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            sts = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
            ps  = map Bounded.p_value sts
        in  and (zipWith (>=) ps (drop 1 ps))

  , QC.testProperty "Paired: p_value monotone nonincreasing" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_pair) $ \ps ->
        let cfg = ok (P.config 0.0 1.0 1.0e-3 b)
            sts = scanl (P.update cfg) (P.initial cfg) ps
            pv  = map P.p_value sts
        in  and (zipWith (>=) pv (drop 1 pv))

  , QC.testProperty "Bounded: p_value in [0, 1], evalue below sup" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll (QC.listOf unit_double) $ \xs ->
        let cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 b)
            sts = scanl (Bounded.update cfg) (Bounded.initial cfg) xs
        in  all (\s -> let p = Bounded.p_value s
                       in  p >= 0 && p <= 1 &&
                           Bounded.log_evalue s
                             <= Bounded.log_evalue_sup s) sts

  , QC.testProperty "Bernoulli: p_value in [0, 1], evalue below sup" $
      QC.forAll arb_bettor $ \b ->
      QC.forAll QC.arbitrary $ \xs ->
        let cfg = ok (Bern.config 0.05 1.0e-3 b)
            sts = scanl (Bern.update cfg) (Bern.initial cfg)
                    (xs :: [Bool])
        in  all (\s -> let p = Bern.p_value s
                       in  p >= 0 && p <= 1 &&
                           Bern.log_evalue s
                             <= Bern.log_evalue_sup s) sts
  ]
