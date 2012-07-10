module BlackScholes where

import SDE

type Rate = Double
type Volatility = Double

data BlackScholes = BS !Rate !Volatility 

instance SDE BlackScholes where
  f (BS rate _) _ w_i = rate *  w_i
  g (BS _ vol) _ w_i = vol * w_i
  partgoverparty s t_i w_i = undefined --TODO: Define to use Milstein method
