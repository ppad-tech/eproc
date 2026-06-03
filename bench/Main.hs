{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bettor as B
import qualified Numeric.Eproc.Bounded as Bounded
import qualified Numeric.Eproc.Paired as P
import Criterion.Main

-- all relevant fields are strict (and UNPACK'd for the doubles), so
-- WHNF == NF for these types. orphan instances keep the library API
-- untouched.
instance NFData Bounded.State    where rnf !_ = ()
instance NFData P.State   where rnf !_ = ()
instance NFData Bounded.Verdict  where rnf !_ = ()

main :: IO ()
main = defaultMain [
    update
  , decide
  , stream
  , twosample
  ]

update :: Benchmark
update =
  let !cfg_f = Bounded.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = Bounded.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = Bounded.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st_f  = Bounded.initial cfg_f
      !st_a  = Bounded.initial cfg_a
      !st_o  = Bounded.initial cfg_o
      !x     = 0.7
  in  bgroup "Bounded.update (one step)" [
          bench "fixed"  $ nf (Bounded.update cfg_f st_f) x
        , bench "agrapa" $ nf (Bounded.update cfg_a st_a) x
        , bench "ons"    $ nf (Bounded.update cfg_o st_o) x
        ]

decide :: Benchmark
decide =
  let !cfg = Bounded.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st  = Bounded.initial cfg
  in  bgroup "Bounded.decide" [
          bench "initial state" $ nf (Bounded.decide cfg) st
        ]

stream :: Benchmark
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = Bounded.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = Bounded.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = Bounded.config 0.5 0.0 1.0 1.0e-3 B.Ons
      run_m cfg = foldl' (Bounded.update cfg) (Bounded.initial cfg)
  in  bgroup "Bounded.update (1000-sample fold)" [
          bench "fixed"  $ nf (run_m cfg_f) xs
        , bench "agrapa" $ nf (run_m cfg_a) xs
        , bench "ons"    $ nf (run_m cfg_o) xs
        ]

twosample :: Benchmark
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = P.config 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = P.config 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = P.config 0.0 1.0 1.0e-3 B.Ons
      run_t cfg = foldl' (P.update cfg) (P.initial cfg)
  in  bgroup "Paired.update (1000-sample fold)" [
          bench "fixed"  $ nf (run_t cfg_f) ps
        , bench "agrapa" $ nf (run_t cfg_a) ps
        , bench "ons"    $ nf (run_t cfg_o) ps
        ]
