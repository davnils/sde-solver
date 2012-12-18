{-# Language BangPatterns #-}

module Numeric.DSDE.SDE.GeometricBrownian where

import Numeric.DSDE.SDE

-- | Geometric Brownian motion with rate r and diffusion sigma.
data GeometricBrownian p = GB !p !p

instance SDE GeometricBrownian where
  {-# SPECIALIZE INLINE f :: GeometricBrownian Double -> Double -> Double -> Double #-}
  f !(GB rate _) _ !w_i = rate *  w_i
  {-# SPECIALIZE INLINE g :: GeometricBrownian Double -> Double -> Double -> Double #-}
  g !(GB _ sigma) _ !w_i = sigma * w_i
  {-# SPECIALIZE INLINE partgoverparty :: GeometricBrownian Double -> Double -> Double -> Double #-}
  partgoverparty !(GB _ sigma) _ _ = sigma
