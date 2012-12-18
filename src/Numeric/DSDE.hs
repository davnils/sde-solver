{-# LANGUAGE ConstraintKinds #-}
--------------------------------------------------------------------
-- |
-- Module : Numeric.DSDE
--
-- Main entry point in the distributed SDE solver.
--
-- This module provides wrappers for the most common use cases, 
-- and the user only needs to choose if to perform a distributed calculation
-- and supply the problem parameters.
--
-- Here follows an example of solving the Langevin equation over
-- the interval [0,2] with the result being written to a file on the main MPI node.
-- This produces a file with 1000 entries, corresponding to the final value
-- in all realisations.
--
-- @
--    let r = 1.0
--        sigma = 0.1
--        y_0 = 3.0
--        end = 0.2
--        stepSize = 0.01
--        runs = 1000
--    withSolver Milstein (distribute (Langevin r sigma) y_0 end stepSize runs) >>=
--      writeResult "out"
-- @

module Numeric.DSDE (
  -- * Abstract solver interface
  distribute,
  local,
  SDEResult(..),

  -- ** Supplied SDE Solvers 
  Milstein(..),
  EulerMaruyama(..),

  -- * Helper functions
  withSolver, writeResult
) where

import qualified Data.Vector.Unboxed as V
import Numeric.DSDE.Distribute
import GHC.Conc
import Numeric.DSDE.RNG
import Numeric.DSDE.SDE
import Numeric.DSDE.SDESolver
import System.IO

-- | distribute: Perform a distributed MPI calculation over a set of hosts supplied through the MPI environment.
--   Each host is fully utilized by running a number of GHC threads equivalent to the number cores available.
--   Internally it uses the highly performant MWC PRNG over the IO monad.
--
--   local: Skips the MPI runtime and only runs locally on GHC threads.
distribute, local :: (SDE sde, SDESolver solver)
  => sde Double    -- ^ SDE Instance
  -> Double        -- ^ Initial y_0 value
  -> Double        -- ^ End of interval, evaluating over [0, end]
  -> Double        -- ^ Step size used by solver
  -> Int           -- ^ Number of realisations
  -> solver        -- ^ Solving method
  -> IO SDEResult  -- ^ Result 

distribute = evalWrapper [Distr MPI]
local = evalWrapper [] 

-- | Wrapper function providing access to the internal API.
evalWrapper :: (SDE sde, SDESolver solver) => [DistributeInstance IO] -> sde Double -> Double -> Double -> Double -> Int -> solver -> IO SDEResult
evalWrapper distr sde y_0 end step runs solver = do
  let rng = initialize
  cores <- getNumCapabilities
  evaluate (distr, Local cores) (sde, solver, rng, IP (End end) y_0 step runs)

-- | Use the specified solver with an IO action.
withSolver :: (SDESolver solver, Monad m) => solver -> (solver -> m a) ->  m a
withSolver = flip id

-- | Write a 'SDEResult' to the supplied filename, output as ASCII text.
writeResult :: FilePath -> SDEResult -> IO ()
writeResult file result = withFile file WriteMode $ output result
  where
  output (Scalar value count) h = hPutStr h $ unwords [show value, show count]
  output (Distribution samples) h = V.mapM_ (hPrint h) samples
