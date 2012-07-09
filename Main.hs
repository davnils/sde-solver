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
  (start:steps:step:rate:vol:simulations:[]) <- map read <$> getArgs :: IO [Double]
  let [steps', simulations'] = map floor [steps, simulations]
  rng <- initialize 4712
  evaluate [Local] (BS rate vol, EulerMaruyama, rng, Steps steps', start, step, simulations') >>= print
