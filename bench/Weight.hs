{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq
import qualified Statistics.EProcess as E
import qualified Statistics.EProcess.Bettor as B
import qualified Statistics.EProcess.Mean as M
import qualified Statistics.EProcess.TwoSample as TS
import Weigh

instance NFData B.AGRAPA      where rnf !_ = ()
instance NFData B.ONS         where rnf !_ = ()
instance NFData (M.State s)   where rnf !_ = ()
instance NFData (TS.State s)  where rnf !_ = ()
instance NFData M.Verdict     where rnf !_ = ()

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  update
  decide
  stream
  twosample

update :: Weigh ()
update =
  let !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (const (E.fixed 0.5))
                :: M.Config ()
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.agrapa
                :: M.Config B.AGRAPA
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.ons
                :: M.Config B.ONS
      !stF  = M.initial cfgF
      !stA  = M.initial cfgA
      !stO  = M.initial cfgO
  in  wgroup "Mean.update (one step)" $ do
        func "fixed"  (M.update cfgF stF) 0.7
        func "agrapa" (M.update cfgA stA) 0.7
        func "ons"    (M.update cfgO stO) 0.7

decide :: Weigh ()
decide =
  let !cfg = M.config 0.5 0.0 1.0 1.0e-3 E.ons :: M.Config B.ONS
      !st  = M.initial cfg
  in  wgroup "Mean.decide" $ do
        func "initial state" (M.decide cfg) st

stream :: Weigh ()
stream =
  let !xs   = force (take 1000 (cycle [0.3, 0.7]))
      !cfgF = M.config 0.5 0.0 1.0 1.0e-3 (const (E.fixed 0.5))
                :: M.Config ()
      !cfgA = M.config 0.5 0.0 1.0 1.0e-3 E.agrapa
                :: M.Config B.AGRAPA
      !cfgO = M.config 0.5 0.0 1.0 1.0e-3 E.ons
                :: M.Config B.ONS
      runM cfg = foldl' (M.update cfg) (M.initial cfg)
  in  wgroup "Mean.update (1000-sample fold)" $ do
        func "fixed"  (runM cfgF) xs
        func "agrapa" (runM cfgA) xs
        func "ons"    (runM cfgO) xs

twosample :: Weigh ()
twosample =
  let !ps   = force (take 1000 (cycle [(0.3, 0.7), (0.7, 0.3)]))
      !cfgF = TS.config 0.0 1.0 1.0e-3 (const (E.fixed 0.5))
                :: TS.Config ()
      !cfgA = TS.config 0.0 1.0 1.0e-3 E.agrapa
                :: TS.Config B.AGRAPA
      !cfgO = TS.config 0.0 1.0 1.0e-3 E.ons
                :: TS.Config B.ONS
      runT cfg = foldl' (TS.update cfg) (TS.initial cfg)
  in  wgroup "TwoSample.update (1000-sample fold)" $ do
        func "fixed"  (runT cfgF) ps
        func "agrapa" (runT cfgA) ps
        func "ons"    (runT cfgO) ps
