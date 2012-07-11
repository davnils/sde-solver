{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module SDESolver where

import RNG
import SDE

data EulerMaruyama = EulerMaruyama
data Milstein = Milstein

class SDESolver a where
  w_iplus1 :: (Monad m, SDE sde, RNGGen rng m) => a -> sde -> rng -> Double -> Double -> Double -> m Double
  solverName :: a -> String

instance SDESolver EulerMaruyama where
  w_iplus1 _ sde rng t_i w_i deltat = getRand rng >>= \rand -> return $
                            w_i
                            + f sde t_i w_i * deltat
                            + g sde t_i w_i * deltaB rand
    where deltaB r = sqrt deltat * r

  solverName _ = "Euler-Maruyama"

instance SDESolver Milstein where
  w_iplus1 = undefined
  solverName _ = "Milstein"
