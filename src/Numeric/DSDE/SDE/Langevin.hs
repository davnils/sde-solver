{-# Language BangPatterns #-}

module Numeric.DSDE.SDE.Langevin where

import Numeric.DSDE.SDE

data Langevin p = Langevin !p !p

instance SDE Langevin where
  {-# SPECIALIZE INLINE f :: Langevin Double -> Double -> Double -> Double #-}
  f !(Langevin r _) _ !w_i = -r * w_i
  {-# SPECIALIZE INLINE g :: Langevin Double -> Double -> Double -> Double #-}
  g !(Langevin _ sigma) _ _ = sigma
  {-# SPECIALIZE INLINE partgoverparty :: Langevin Double -> Double -> Double -> Double #-}
  partgoverparty _ _ _ = 0
