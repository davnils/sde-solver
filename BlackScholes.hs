{-# Language BangPatterns #-}

module BlackScholes where

import SDE

data BlackScholes p = BS !p !p

instance SDE BlackScholes where
  {-# SPECIALIZE INLINE f :: BlackScholes Double -> Double -> Double -> Double #-}
  f !(BS rate _) _ !w_i = rate *  w_i
  {-# SPECIALIZE INLINE g :: BlackScholes Double -> Double -> Double -> Double #-}
  g !(BS _ vol) _ !w_i = vol * w_i
  partgoverparty _ _ _ = undefined --TODO: Define to use Milstein method
