{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Statistics.EProcess as E
import qualified Statistics.EProcess.Mean as M
import qualified Statistics.EProcess.TwoSample as TS
import Criterion.Main

-- all relevant fields are strict (and UNPACK'd for the doubles), so
-- WHNF == NF for these types. orphan instances keep the library API
-- untouched.
instance NFData M.State    where rnf !_ = ()
instance NFData TS.State   where rnf !_ = ()
instance NFData M.Verdict  where rnf !_ = ()

main :: IO ()
main = defaultMain [
    update
  , decide
  , stream
  , twosample
  ]

update :: Benchmark
update =
  let !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      !stF  = M.initial cfgF
      !stA  = M.initial cfgA
      !stO  = M.initial cfgO
      !x    = 0.7
  in  bgroup "Mean.update (one step)" [
          bench "fixed"  $ nf (M.update cfgF stF) x
        , bench "agrapa" $ nf (M.update cfgA stA) x
        , bench "ons"    $ nf (M.update cfgO stO) x
        ]

decide :: Benchmark
decide =
  let !cfg = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      !st  = M.initial cfg
  in  bgroup "Mean.decide" [
          bench "initial state" $ nf (M.decide cfg) st
        ]

stream :: Benchmark
stream =
  let !xs   = force (take 1000 (cycle [0.3, 0.7]))
      !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      runM cfg = foldl' (M.update cfg) (M.initial cfg)
  in  bgroup "Mean.update (1000-sample fold)" [
          bench "fixed"  $ nf (runM cfgF) xs
        , bench "agrapa" $ nf (runM cfgA) xs
        , bench "ons"    $ nf (runM cfgO) xs
        ]

twosample :: Benchmark
twosample =
  let !ps   = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfgF = TS.config 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = TS.config 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = TS.config 0.0 1.0 1.0e-3 E.Ons
      runT cfg = foldl' (TS.update cfg) (TS.initial cfg)
  in  bgroup "TwoSample.update (1000-sample fold)" [
          bench "fixed"  $ nf (runT cfgF) ps
        , bench "agrapa" $ nf (runT cfgA) ps
        , bench "ons"    $ nf (runT cfgO) ps
        ]
