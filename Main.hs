module Main where

import BlackScholes
import Control.Applicative
import Control.Monad.Identity
-- import qualified Data.Array.Accelerate as Acc
import Distribute (DistributeInstance(..), Local(..), MPI(..),
                   Accuracy(..), evaluate, InstanceParams(IP))
import GHC.Conc
import RNG
import SDESolver
import System.Environment

main :: IO ()
main = do
  (start:steps:step:rate:vol:simulations:[]) <- map read <$> getArgs :: IO [Double]
  let [steps', simulations'] = map floor [steps, simulations]
  let rng = initialize
  cores <- getNumCapabilities
  res <- evaluate ([], Local cores) (BS rate vol, EulerMaruyama, rng, IP (Steps steps') start step simulations')
  print res
