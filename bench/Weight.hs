{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bernoulli as Bern
import qualified Numeric.Eproc.Bounded as Bounded
import qualified Numeric.Eproc.Paired as P
import Weigh

instance NFData Bounded.State    where rnf !_ = ()
instance NFData P.State          where rnf !_ = ()
instance NFData Bern.State       where rnf !_ = ()
instance NFData Bounded.Verdict  where rnf !_ = ()

-- partial helper for benches: configs here are hardcoded valid.
ok :: Either e a -> a
ok (Right x) = x
ok (Left _)  = error "weigh: invalid config"

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  update
  decide
  stream
  twosample
  bern_update
  bern_stream

update :: Weigh ()
update =
  let !cfg_f = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      !st_f  = Bounded.initial cfg_f
      !st_a  = Bounded.initial cfg_a
      !st_o  = Bounded.initial cfg_o
  in  wgroup "Bounded.update (one step)" $ do
        func "fixed"    (Bounded.update cfg_f st_f) 0.7
        func "adaptive" (Bounded.update cfg_a st_a) 0.7
        func "newton"   (Bounded.update cfg_o st_o) 0.7

decide :: Weigh ()
decide =
  let !cfg = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      !st  = Bounded.initial cfg
  in  wgroup "Bounded.decide" $ do
        func "initial state" (Bounded.decide cfg) st

stream :: Weigh ()
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Newton)
      run_m cfg = foldl' (Bounded.update cfg) (Bounded.initial cfg)
  in  wgroup "Bounded.update (1000-sample fold)" $ do
        func "fixed"    (run_m cfg_f) xs
        func "adaptive" (run_m cfg_a) xs
        func "newton"   (run_m cfg_o) xs

twosample :: Weigh ()
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = ok (P.config 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5))
      !cfg_a = ok (P.config 0.0 1.0 1.0e-3 Bounded.Adaptive)
      !cfg_o = ok (P.config 0.0 1.0 1.0e-3 Bounded.Newton)
      run_t cfg = foldl' (P.update cfg) (P.initial cfg)
  in  wgroup "Paired.update (1000-sample fold)" $ do
        func "fixed"    (run_t cfg_f) ps
        func "adaptive" (run_t cfg_a) ps
        func "newton"   (run_t cfg_o) ps

bern_update :: Weigh ()
bern_update =
  let !cfg_f = ok (Bern.config 0.05 1.0e-3 (Bern.Fixed 5.0))
      !cfg_a = ok (Bern.config 0.05 1.0e-3 Bern.Adaptive)
      !cfg_o = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
      !st_f  = Bern.initial cfg_f
      !st_a  = Bern.initial cfg_a
      !st_o  = Bern.initial cfg_o
  in  wgroup "Bernoulli.update (one step)" $ do
        func "fixed"    (Bern.update cfg_f st_f) True
        func "adaptive" (Bern.update cfg_a st_a) True
        func "newton"   (Bern.update cfg_o st_o) True

bern_stream :: Weigh ()
bern_stream =
  let !xs    = force (take 1000 (cycle [True, False]))
      !cfg_f = ok (Bern.config 0.05 1.0e-3 (Bern.Fixed 5.0))
      !cfg_a = ok (Bern.config 0.05 1.0e-3 Bern.Adaptive)
      !cfg_o = ok (Bern.config 0.05 1.0e-3 Bern.Newton)
      run_b cfg = foldl' (Bern.update cfg) (Bern.initial cfg)
  in  wgroup "Bernoulli.update (1000-sample fold)" $ do
        func "fixed"    (run_b cfg_f) xs
        func "adaptive" (run_b cfg_a) xs
        func "newton"   (run_b cfg_o) xs
