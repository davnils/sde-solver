module Main where

import BlackScholes
import Control.Applicative
import Distribute
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
  evaluate [Distr $ Local cores] (BS rate vol, EulerMaruyama, rng, Steps steps', start, step, simulations') >>= print
