{-# LANGUAGE ConstraintKinds #-}

module Numeric.DSDE.SDE where

-- | Type of result and intermediate variables used when solving an SDE problem.
type Parameter a = (Floating a, Fractional a, Num a)

-- | Describes an SDE problem. The partial derivate is used by advanced solvers
--   such as the 'Milstein' method.
class SDE a where
  f,g,partgoverparty :: Parameter p => a p -> p -> p -> p
