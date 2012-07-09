module BlackScholes where

import SDE

type Rate = Double
type Volatility = Double

data BlackScholes = BS !Rate !Volatility 

instance SDE BlackScholes where
  f s t_i w_i = undefined
  g s t_i w_i = undefined
  partgoverparty s t_i w_i = undefined --TODO: Define to use Milstein method
