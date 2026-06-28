{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bernoulli as Bern
import qualified Numeric.Eproc.Bounded as Bounded
import qualified Numeric.Eproc.Paired as P
import Criterion.Main

-- all relevant fields are strict (and UNPACK'd for the doubles), so
-- WHNF == NF for these types. orphan instances keep the library API
-- untouched.
instance NFData Bounded.State    where rnf !_ = ()
instance NFData P.State          where rnf !_ = ()
instance NFData Bern.State       where rnf !_ = ()
instance NFData Bounded.Verdict  where rnf !_ = ()

-- partial helper for benches: configs here are hardcoded valid, so a
-- 'Left' would be a bench-suite bug.
ok :: Either e a -> a
ok (Right x) = x
ok (Left _)  = error "bench: invalid config"

main :: IO ()
main = defaultMain [
    update
  , decide
  , stream
  , twosample
  , bern_update
  , bern_stream
  ]

update :: Benchmark
update =
  let !cfg_f = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      !st_f  = Bounded.initial cfg_f
      !st_a  = Bounded.initial cfg_a
      !st_o  = Bounded.initial cfg_o
      !x     = 0.7
  in  bgroup "Bounded.update (one step)" [
          bench "fixed"    $ nf (Bounded.update cfg_f st_f) x
        , bench "adaptive" $ nf (Bounded.update cfg_a st_a) x
        , bench "newton"   $ nf (Bounded.update cfg_o st_o) x
        ]

decide :: Benchmark
decide =
  let !cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      !st  = Bounded.initial cfg
  in  bgroup "Bounded.decide" [
          bench "initial state" $ nf (Bounded.decide cfg) st
        ]

stream :: Benchmark
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      run_m cfg = foldl' (Bounded.update cfg) (Bounded.initial cfg)
  in  bgroup "Bounded.update (1000-sample fold)" [
          bench "fixed"    $ nf (run_m cfg_f) xs
        , bench "adaptive" $ nf (run_m cfg_a) xs
        , bench "newton"   $ nf (run_m cfg_o) xs
        ]

twosample :: Benchmark
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = ok (P.config 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (P.config 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (P.config 0.0 1.0 1.0e-3 Bounded.Newton)
      run_t cfg = foldl' (P.update cfg) (P.initial cfg)
  in  bgroup "Paired.update (1000-sample fold)" [
          bench "fixed"    $ nf (run_t cfg_f) ps
        , bench "adaptive" $ nf (run_t cfg_a) ps
        , bench "newton"   $ nf (run_t cfg_o) ps
        ]

bern_update :: Benchmark
bern_update =
  let !cfg_f = ok (Bern.config 0.05 1.0e-3 (Bern.Fixed 5.0))
      !cfg_a = ok (Bern.config 0.05 1.0e-3 Bern.Adaptive)
      !cfg_o = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
      !st_f  = Bern.initial cfg_f
      !st_a  = Bern.initial cfg_a
      !st_o  = Bern.initial cfg_o
  in  bgroup "Bernoulli.update (one step)" [
          bench "fixed"    $ nf (Bern.update cfg_f st_f) True
        , bench "adaptive" $ nf (Bern.update cfg_a st_a) True
        , bench "newton"   $ nf (Bern.update cfg_o st_o) True
        ]

bern_stream :: Benchmark
bern_stream =
  let !xs    = force (take 1000 (cycle [True, False]))
      !cfg_f = ok (Bern.config 0.05 1.0e-3 (Bern.Fixed 5.0))
      !cfg_a = ok (Bern.config 0.05 1.0e-3 Bern.Adaptive)
      !cfg_o = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
      run_b cfg = foldl' (Bern.update cfg) (Bern.initial cfg)
  in  bgroup "Bernoulli.update (1000-sample fold)" [
          bench "fixed"    $ nf (run_b cfg_f) xs
        , bench "adaptive" $ nf (run_b cfg_a) xs
        , bench "newton"   $ nf (run_b cfg_o) xs
        ]
