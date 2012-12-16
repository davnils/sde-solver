module Main where

import Control.Applicative
import Numeric.DSDE
import Numeric.DSDE.SDE.Langevin
import System.Environment

main :: IO ()
main = do
  (start:end:step:r:sigma:simulations:[]) <- map read <$> getArgs
  res <- withSolver Milstein $ distribute (Langevin r sigma) start end step (floor simulations)
  writeResult "out" res
