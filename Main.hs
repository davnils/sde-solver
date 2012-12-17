module Main where

import Control.Applicative
import Control.Monad.State
import Numeric.DSDE
import Numeric.DSDE.Distribute
import Numeric.DSDE.RNG
import Numeric.DSDE.SDE.Langevin
import System.Environment
import qualified System.Random.Mersenne.Pure64 as MT

main :: IO ()
main = do
  (start:end:step:r:sigma:simulations:[]) <- map read <$> getArgs

  -- let init = initialize :: Maybe Int -> State MT.PureMT MT.PureMT
  -- print $ runState (evaluate ([], Local 4) ((Langevin r sigma), Milstein, init, IP (End end) start step (floor simulations))) (MT.pureMT 0)

  -- res <- withSolver Milstein $ distribute (Langevin r sigma) start end step (floor simulations)
  -- writeResult "out" res
