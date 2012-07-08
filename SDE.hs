module SDE where

type TimeStep = Double

class SDE a where
  f :: a -> TimeStep -> Double -> Double
  g :: a -> TimeStep -> Double -> Double
  partgoverparty :: a -> TimeStep -> Double -> Double
