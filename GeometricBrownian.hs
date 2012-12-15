{-# Language BangPatterns #-}

module GeometricBrownian where

import SDE

data GeometricBrownian p = GB !p !p

instance SDE GeometricBrownian where
  {-# SPECIALIZE INLINE f :: GeometricBrownian Double -> Double -> Double -> Double #-}
  f !(GB rate _) _ !w_i = rate *  w_i
  {-# SPECIALIZE INLINE g :: GeometricBrownian Double -> Double -> Double -> Double #-}
  g !(GB _ sigma) _ !w_i = sigma * w_i
  {-# SPECIALIZE INLINE partgoverparty :: GeometricBrownian Double -> Double -> Double -> Double #-}
  partgoverparty !(GB _ sigma) _ _ = sigma
