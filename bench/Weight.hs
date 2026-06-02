{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Statistics.EProcess as E
import qualified Statistics.EProcess.Mean as M
import qualified Statistics.EProcess.TwoSample as TS
import Weigh

instance NFData M.State    where rnf !_ = ()
instance NFData TS.State   where rnf !_ = ()
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
  let !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      !stF  = M.initial cfgF
      !stA  = M.initial cfgA
      !stO  = M.initial cfgO
  in  wgroup "Mean.update (one step)" $ do
        func "fixed"  (M.update cfgF stF) 0.7
        func "agrapa" (M.update cfgA stA) 0.7
        func "ons"    (M.update cfgO stO) 0.7

decide :: Weigh ()
decide =
  let !cfg = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      !st  = M.initial cfg
  in  wgroup "Mean.decide" $ do
        func "initial state" (M.decide cfg) st

stream :: Weigh ()
stream =
  let !xs   = force (take 1000 (cycle [0.3, 0.7]))
      !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.Ons
      runM cfg = foldl' (M.update cfg) (M.initial cfg)
  in  wgroup "Mean.update (1000-sample fold)" $ do
        func "fixed"  (runM cfgF) xs
        func "agrapa" (runM cfgA) xs
        func "ons"    (runM cfgO) xs

twosample :: Weigh ()
twosample =
  let !ps   = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfgF = TS.config 0.0 1.0 1.0e-3 (E.Fixed 0.5)
      !cfgA = TS.config 0.0 1.0 1.0e-3 E.Agrapa
      !cfgO = TS.config 0.0 1.0 1.0e-3 E.Ons
      runT cfg = foldl' (TS.update cfg) (TS.initial cfg)
  in  wgroup "TwoSample.update (1000-sample fold)" $ do
        func "fixed"  (runT cfgF) ps
        func "agrapa" (runT cfgA) ps
        func "ons"    (runT cfgO) ps
