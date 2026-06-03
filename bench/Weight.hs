{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Numeric.Eproc.Bettor as B
import qualified Numeric.Eproc.Mean as M
import qualified Numeric.Eproc.Paired as P
import Weigh

instance NFData M.State    where rnf !_ = ()
instance NFData P.State   where rnf !_ = ()
instance NFData M.Verdict  where rnf !_ = ()

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  update
  decide
  stream
  twosample

update :: Weigh ()
update =
  let !cfg_f = M.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st_f  = M.initial cfg_f
      !st_a  = M.initial cfg_a
      !st_o  = M.initial cfg_o
  in  wgroup "Mean.update (one step)" $ do
        func "fixed"  (M.update cfg_f st_f) 0.7
        func "agrapa" (M.update cfg_a st_a) 0.7
        func "ons"    (M.update cfg_o st_o) 0.7

decide :: Weigh ()
decide =
  let !cfg = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      !st  = M.initial cfg
  in  wgroup "Mean.decide" $ do
        func "initial state" (M.decide cfg) st

stream :: Weigh ()
stream =
  let !xs    = force (take 1000 (cycle [0.3, 0.7]))
      !cfg_f = M.config 0.5 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = M.config 0.5 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = M.config 0.5 0.0 1.0 1.0e-3 B.Ons
      run_m cfg = foldl' (M.update cfg) (M.initial cfg)
  in  wgroup "Mean.update (1000-sample fold)" $ do
        func "fixed"  (run_m cfg_f) xs
        func "agrapa" (run_m cfg_a) xs
        func "ons"    (run_m cfg_o) xs

twosample :: Weigh ()
twosample =
  let !ps    = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfg_f = P.config 0.0 1.0 1.0e-3 (B.Fixed 0.5)
      !cfg_a = P.config 0.0 1.0 1.0e-3 B.Agrapa
      !cfg_o = P.config 0.0 1.0 1.0e-3 B.Ons
      run_t cfg = foldl' (P.update cfg) (P.initial cfg)
  in  wgroup "Paired.update (1000-sample fold)" $ do
        func "fixed"  (run_t cfg_f) ps
        func "agrapa" (run_t cfg_a) ps
        func "ons"    (run_t cfg_o) ps
