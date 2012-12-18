{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             BangPatterns, ConstraintKinds #-}

module Numeric.DSDE.SDESolver where

import Numeric.DSDE.SDE.GeometricBrownian
import Numeric.DSDE.RNG
import Numeric.DSDE.SDE
import qualified System.Random.MWC as M

-- | The Euler-Maruyama solving method. Order 1/2.
data EulerMaruyama = EulerMaruyama

-- | The Milstein solving method. Order 1.
data Milstein = Milstein

-- | Type class describing a method of solving SDE problems.
--    Defined by the next value produced in a solving sequence.
class SDESolver a where
  w_iplus1 :: (Monad m, SDE sde, RNGGen rng m p, Parameter p) =>
    a -> sde p -> rng -> p -> p -> p -> m p
  solverName :: a -> String

instance SDESolver EulerMaruyama where
  {-# INLINE w_iplus1 #-}
  {-# SPECIALIZE w_iplus1 :: EulerMaruyama -> GeometricBrownian Double -> M.GenIO -> Double -> Double -> Double -> IO Double #-}
  w_iplus1 _ !sde !rng !t_i !w_i !deltat = getRand rng >>= \rand -> return $
                            w_i
                            + f sde t_i w_i * deltat
                            + g sde t_i w_i * deltaB rand
    where deltaB r = sqrt deltat * r

  solverName _ = "Euler-Maruyama"

instance SDESolver Milstein where
  w_iplus1 _ !sde !rng !t_i !w_i !deltat = getRand rng >>= \rand -> return $
                            w_i
                            + f sde t_i w_i * deltat
                            + g' * deltaB rand
                            + g'/2 * partgoverparty sde t_i w_i  * (deltaB rand^^(2 :: Integer) - deltat)
    where
    deltaB r = sqrt deltat * r
    g' = g sde t_i w_i
  solverName _ = "Milstein"
