module Main where

import BlackScholes
import Control.Applicative
import Distribute (DistributeInstance(..), Local(..), MPI(..),
                   Accuracy(..), evaluate, InstanceParams(IP))
import GHC.Conc
import RNG
import SDESolver
import System.Environment

main :: IO ()
main = do
  -- Temporary glue
  (start:steps:step:rate:vol:simulations:[]) <- map read <$> getArgs :: IO [Double]
  let [steps', simulations'] = map floor [steps, simulations]
  let rng = initialize
  cores <- getNumCapabilities
  evaluate ([Distr MPI], Local cores) (BS rate vol, EulerMaruyama, rng, IP (Steps steps') start step simulations') >>= print
