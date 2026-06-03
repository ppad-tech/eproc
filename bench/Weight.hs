{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bounded as Bounded
import qualified Numeric.Eproc.Paired as P
import Weigh

instance NFData Bounded.State    where rnf !_ = ()
instance NFData P.State   where rnf !_ = ()
instance NFData Bounded.Verdict  where rnf !_ = ()

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  update
  decide
  stream
  twosample

update :: Weigh ()
update =
  let !cfg_f = Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5)
      !cfg_a = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Agrapa
      !cfg_o = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Ons
      !st_f  = Bounded.initial cfg_f
      !st_a  = Bounded.initial cfg_a
      !st_o  = Bounded.initial cfg_o
  in  wgroup "Bounded.update (one step)" $ do
        func "fixed"  (Bounded.update cfg_f st_f) 0.7
        func "agrapa" (Bounded.update cfg_a st_a) 0.7
        func "ons"    (Bounded.update cfg_o st_o) 0.7

decide :: Weigh ()
decide =
  let !cfg = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Ons
      !st  = Bounded.initial cfg
  in  wgroup "Bounded.decide" $ do
        func "initial state" (Bounded.decide cfg) st

stream :: Weigh ()
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = Bounded.config 0.5 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5)
      !cfg_a = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Agrapa
      !cfg_o = Bounded.config 0.5 0.0 1.0 1.0e-3 Bounded.Ons
      run_m cfg = foldl' (Bounded.update cfg) (Bounded.initial cfg)
  in  wgroup "Bounded.update (1000-sample fold)" $ do
        func "fixed"  (run_m cfg_f) xs
        func "agrapa" (run_m cfg_a) xs
        func "ons"    (run_m cfg_o) xs

twosample :: Weigh ()
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = P.config 0.0 1.0 1.0e-3 (Bounded.Fixed 0.5)
      !cfg_a = P.config 0.0 1.0 1.0e-3 Bounded.Agrapa
      !cfg_o = P.config 0.0 1.0 1.0e-3 Bounded.Ons
      run_t cfg = foldl' (P.update cfg) (P.initial cfg)
  in  wgroup "Paired.update (1000-sample fold)" $ do
        func "fixed"  (run_t cfg_f) ps
        func "agrapa" (run_t cfg_a) ps
        func "ons"    (run_t cfg_o) ps
