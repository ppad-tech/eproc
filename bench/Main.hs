{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bettor as B
import qualified Numeric.Eproc.Mean as M
import qualified Numeric.Eproc.Test as T
import Criterion.Main

-- all relevant fields are strict (and UNPACK'd for the doubles), so
-- WHNF == NF for these types. orphan instances keep the library API
-- untouched.
instance NFData M.State    where rnf !_ = ()
instance NFData T.State   where rnf !_ = ()
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
  let !cfg_f = M.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st_f  = M.initial cfg_f
      !st_a  = M.initial cfg_a
      !st_o  = M.initial cfg_o
      !x     = 0.7
  in  bgroup "Mean.update (one step)" [
          bench "fixed"  $ nf (M.update cfg_f st_f) x
        , bench "agrapa" $ nf (M.update cfg_a st_a) x
        , bench "ons"    $ nf (M.update cfg_o st_o) x
        ]

decide :: Benchmark
decide =
  let !cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st  = M.initial cfg
  in  bgroup "Mean.decide" [
          bench "initial state" $ nf (M.decide cfg) st
        ]

stream :: Benchmark
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = M.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      run_m cfg = foldl' (M.update cfg) (M.initial cfg)
  in  bgroup "Mean.update (1000-sample fold)" [
          bench "fixed"  $ nf (run_m cfg_f) xs
        , bench "agrapa" $ nf (run_m cfg_a) xs
        , bench "ons"    $ nf (run_m cfg_o) xs
        ]

twosample :: Benchmark
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = T.config 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = T.config 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = T.config 0.0 1.0 1.0e-3 B.Ons
      run_t cfg = foldl' (T.update cfg) (T.initial cfg)
  in  bgroup "Test.update (1000-sample fold)" [
          bench "fixed"  $ nf (run_t cfg_f) ps
        , bench "agrapa" $ nf (run_t cfg_a) ps
        , bench "ons"    $ nf (run_t cfg_o) ps
        ]
