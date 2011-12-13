{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module SDE where

import Control.Applicative
import Control.Monad.Identity
import Data.Foldable (foldlM)
import Data.List (foldl')
import Data.Maybe
import RNG

type TimeStep = Double

data Accuracy = StepSize Double TimeStep | Steps Int

class SDE a where
	step :: (Monad m, RNGGen g m) => a -> g -> Double -> m Double
	config :: a -> (Double, Accuracy)
	parseCmd :: [String] -> a

evalValue :: (SDE a, Monad m, RNGGen g m) => a -> g -> m Double
evalValue sde rng = foldM (\acc _ -> step sde rng acc) start [1..steps]
	where
	(start, accuracy') = config sde
	steps = case accuracy' of
		--Assumes that step size evenly divides the interval length
		StepSize dt endTime -> floor $ endTime / dt
		Steps n -> n
