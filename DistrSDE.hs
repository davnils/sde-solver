{-# LANGUAGE ConstraintKinds #-}

module DistrSDE (distribute, local, withSolver, writeResult, SDEResult(..), Milstein(..), EulerMaruyama(..)) where

import qualified Data.Vector.Unboxed as V
import Distribute
import GHC.Conc
import RNG
import SDE
import SDESolver
import System.IO

distribute, local :: (SDE sde, SDESolver solver) => sde Double -> Double -> Double -> Double -> Int -> solver -> IO SDEResult
distribute = evalWrapper [Distr MPI]
local = evalWrapper [] 

evalWrapper :: (SDE sde, SDESolver solver) => [DistributeInstance IO] -> sde Double -> Double -> Double -> Double -> Int -> solver -> IO SDEResult
evalWrapper distr sde y_0 end step runs solver = do
  let rng = initialize
  cores <- getNumCapabilities
  evaluate (distr, Local cores) (sde, solver, rng, IP (End end) y_0 step runs)

withSolver :: (SDESolver solver, Monad m) => solver -> (solver -> m a) ->  m a
withSolver = flip id

writeResult :: FilePath -> SDEResult -> IO ()
writeResult file result = withFile file WriteMode $ output result
  where
  output (Scalar value count) h = hPutStr h $ unwords [show value, show count]
  output (Distribution samples) h = V.mapM_ (hPrint h) samples
