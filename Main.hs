module Main where

import BlackScholes
import Control.Applicative
import Distribute
import RNG
import SDE
import SDESolver
import System.Environment
import System.IO

main :: IO ()
main = do
  -- Temporary glue
  (start:steps:step:rate:vol:simulations:seed:[]) <- map read <$> getArgs :: IO [Double]
  let [steps', simulations', seed'] = map floor [steps, simulations, seed]
  rng <- initialize seed'
  evaluate [Distr Local] (BS rate vol, EulerMaruyama, rng, Steps steps', start, step, simulations') >>= print
