{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns,
             ExistentialQuantification, FlexibleInstances #-}
module Distribute where

-- TODO: (1) Fix t_i
--       (2) Check if the required bottom-execution can be encoded in the type system

import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import qualified Control.Monad.Parallel as P
import Data.List (foldl')
import Prelude hiding (sum)
import RNG
import SDE
import SDESolver

data DistributeInstance m = forall a. Distribute a m => Distr a

data SDEResult = Scalar Double 
               | NotApplicable
  deriving Show

data Accuracy = End TimeStep
              | Steps Int
  deriving Show

data MPICluster = MPICluster
data GPUAccelerate = GPUAccelerate
data Local = Local Int

type SDEConstraint b c g m = (SDE b, SDESolver c, RNGGen g m)
type SDEInstance b c g m = (b, c, Int -> m g, Accuracy, Double, Double, Int)

class (P.MonadParallel m) => Distribute a m where
  inject :: SDEConstraint b c g m => a -> SDEInstance b c g m -> m (SDEInstance b c g m)
  execute :: SDEConstraint b c g m =>  a -> SDEInstance b c g m -> m SDEResult
  remove :: a -> SDEResult -> m SDEResult

instance Distribute MPICluster IO where
  inject _ _ = undefined
  execute _ _ = return NotApplicable
  remove _ _ = undefined

instance (P.MonadParallel m) => Distribute Local m where
  inject _ = id <$> return
  execute (Local cores) (!sde, !solver, !rng, !accuracy, !start, !deltat, !simulations) = 
    Scalar <$> (P.mapM (\r -> rng r >>= thread >>= average perThread) [1..cores] >>= average cores)
    where
      eval rand w_i _ = w_iplus1 solver sde rand 0 w_i deltat
      perThread = floor $ realToFrac simulations / realToFrac cores :: Int
      steps = case accuracy of
        End endTime -> floor $ endTime / deltat
        Steps n -> n
      thread rand = mapM (\_ -> foldM' (eval rand) start [1..steps]) [1..perThread]
      average n solutions = return $ sum solutions / realToFrac n
      sum = foldl' (+) 0

  remove _ = id <$> return

evaluate :: (P.MonadParallel m, SDEConstraint b c g m) => [DistributeInstance m] -> SDEInstance b c g m -> m SDEResult
evaluate [] _ = return NotApplicable
evaluate (Distr method : []) input =
  inject method input >>= execute method >>= remove method
evaluate (Distr method : other) input =
  inject method input >>= evaluate other >>= remove method

foldM' :: (P.MonadParallel m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' func z (x:xs) = do
  z' <- func z x
  z' `seq` foldM' func z' xs
