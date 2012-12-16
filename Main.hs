module Main where

import Control.Applicative
import DSDE
import Langevin
import System.Environment

main :: IO ()
main = do
  (start:end:step:r:sigma:simulations:[]) <- map read <$> getArgs
  res <- withSolver Milstein $ local (Langevin r sigma) start end step (floor simulations)
  writeResult "out" res
