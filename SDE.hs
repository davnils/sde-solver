{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             BangPatterns #-}

module SDE where

import Control.Applicative
import Control.Monad.Identity
import qualified Control.Monad.Parallel as P
import Control.Parallel.Strategies
import Data.Foldable (foldlM)
import Data.List (foldl')
import Data.Maybe
import RNG

type TimeStep = Double

data Accuracy = StepSize Double TimeStep | Steps Int

class SDE a where
        step :: (Monad m, Functor m, P.MonadParallel m, RNGGen g m) => a -> g -> Double -> m Double
        config :: a -> (Double, Accuracy, Int)
        parseCmd :: [String] -> a

evalValue :: (SDE a, Monad m, Functor m, P.MonadParallel m, RNGGen g m) => a -> g -> m Double
evalValue sde rng = foldM (\acc _ -> step sde rng acc) start [1..steps]
        where
        (start, accuracy', _) = config sde
        steps = case accuracy' of
                --Assumes that step size evenly divides the interval length
                StepSize dt endTime -> floor $ endTime / dt
                Steps n -> n

-- Evaluate the SDE 'simulations' number of times and average the results
solve :: (SDE a, Monad m, Functor m, P.MonadParallel m, RNGGen g m) => a -> (Int -> m g) -> Int -> m Double
solve sde getRng seed = do
        super <- getRng seed
        (!rands) <- mapM (\_ -> getRand super >>= getRng . floor . (*1000)) [1..simulations]
        vals' <- P.mapM (evalValue sde) rands
        let vals = foldl' (+) 0 vals'
        return $ vals / realToFrac simulations
        where
        (_, _, simulations) = config sde
