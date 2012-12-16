{-# LANGUAGE ConstraintKinds #-}

module Numeric.DSDE.SDE where

type Parameter a = (Floating a, Fractional a, Num a)

class SDE a where
  f,g,partgoverparty :: Parameter p => a p -> p -> p -> p
