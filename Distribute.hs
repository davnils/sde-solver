{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes, ImpredicativeTypes #-}
module Distribute where

-- TODO: (1) Resolve type problems
--       (2) Implement t_i, deltat and several simulations.
--       (3) Check if the required bottom-execution can be encoded in the type system

import Control.Applicative
import Control.Monad.Identity
import RNG
import SDE
import SDESolver

data SDEResult = Scalar Double 
               | NotApplicable

data Accuracy = StepSize Double TimeStep
              | Steps Int

type SDEInstance m = (SDE b, SDESolver c, RNGGen g m) => (b, c, g, Accuracy, Double)

data MPICluster = MPICluster
data GPUAccelerate = GPUAccelerate
data Local = Local

class (Monad m) => Distribute a m where
    inject :: a -> SDEInstance m -> m (SDEInstance m)
    execute :: a -> SDEInstance m -> m SDEResult
    remove :: a -> SDEResult -> m SDEResult

instance Distribute MPICluster IO where
    inject _ input = undefined
    execute _ _ = return NotApplicable
    remove _ result = undefined

instance Distribute Local IO where
    inject _ = id <$> return
    execute _ (sde, solver, rng, acc, start) = Scalar <$> foldM f start [1..steps]
      where f w_i _ = w_iplus1 solver sde rng undefined w_i undefined
            steps = case acc of
              StepSize dt endTime -> floor $ endTime / dt
              Steps n -> n
    remove _ = id <$> return

evaluate :: (Distribute a m,  Monad m) => [a] -> SDEInstance m -> m SDEResult
evaluate [] _ = return NotApplicable
evaluate (method:[]) input =
    inject method input >>= execute method >>= remove method
evaluate (method:tail) input =
    inject method input >>= evaluate tail >>= remove method
