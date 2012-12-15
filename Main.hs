module Main where

import Control.Applicative
import Control.Monad.Identity
import Control.Parallel.Strategies
-- import qualified Data.Array.Acceler as Acc
import qualified Data.Vector.Unboxed as V
import Data.Vector.Strategies
import Distribute (DistributeInstance(..), Local(..), MPI(..),
                   Accuracy(..), evaluate, InstanceParams(IP), SDEResult(..))
import GeometricBrownian
import GHC.Conc
import Langevin
import RNG
import SDESolver
import System.Environment

main :: IO ()
main = do
  (start:steps:step:r:sigma:simulations:[]) <- map read <$> getArgs :: IO [Double]
  let [steps', simulations'] = map floor [steps, simulations]
  let rng = initialize
  cores <- getNumCapabilities
  Distribution res <- evaluate ([], Local cores) (GB r sigma, Milstein, rng, IP (Steps steps') start step simulations')
  writeFile "sde2" $ unlines $ map show $ V.toList res
