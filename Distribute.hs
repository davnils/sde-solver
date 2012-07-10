{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns, ExistentialQuantification #-}
module Distribute where

-- TODO: (1) Fix t_i
--       (2) Check if the required bottom-execution can be encoded in the type system

import Control.Applicative
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Parallel
import Data.List
import Prelude hiding (sum, mapM)
import RNG
import SDE
import SDESolver

data DistributeInstance a m = forall a. Distribute a m => Distr a

data SDEResult = Scalar Double 
               | NotApplicable
  deriving Show

data Accuracy = End TimeStep
              | Steps Int
  deriving Show

data MPICluster = MPICluster
data GPUAccelerate = GPUAccelerate
data Local = Local

type SDEConstraint b c g m = (SDE b, SDESolver c, RNGGen g m)
type SDEInstance b c g = (b, c, g, Accuracy, Double, Double, Int)

class (MonadParallel m) => Distribute a m where
  inject :: SDEConstraint b c g m => a -> SDEInstance b c g -> m (SDEInstance b c g)
  execute :: SDEConstraint b c g m =>  a -> SDEInstance b c g -> m SDEResult
  remove :: a -> SDEResult -> m SDEResult

instance Distribute MPICluster IO where
  inject _ input = undefined
  execute _ _ = return NotApplicable
  remove _ result = undefined

instance Distribute Local IO where
  inject _ = id <$> return
  execute _ (!sde, !solver, !rng, !accuracy, !start, !deltat, !simulations) = 
    Scalar <$> (mapM single [1..simulations] >>= average simulations)
    where
      f w_i _ = w_iplus1 solver sde rng undefined w_i deltat
      steps = case accuracy of
        End endTime -> floor $ endTime / deltat
        Steps n -> n
      single _ = foldM' f start [1..steps]
      average n solutions = return $ sum solutions / (realToFrac $ n)
      sum = foldl' (+) 0

  remove _ = id <$> return

evaluate :: (MonadParallel m, SDEConstraint b c g m) => [DistributeInstance a m] -> SDEInstance b c g -> m SDEResult
evaluate [] _ = return NotApplicable
evaluate ((Distr method):[]) input =
  inject method input >>= execute method >>= remove method
evaluate ((Distr method):tail) input =
  inject method input >>= evaluate tail >>= remove method

foldM' :: (MonadParallel m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs
