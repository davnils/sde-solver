module BlackScholes where

import SDE

type Rate = Double
type Volatility = Double

data BlackScholes = BS !Rate !Volatility 

instance SDE BlackScholes where
  {-# INLINE f #-}
  f (BS rate _) _ w_i = rate *  w_i
  {-# INLINE g #-}
  g (BS _ vol) _ w_i = vol * w_i
  partgoverparty _ _ _ = undefined --TODO: Define to use Milstein method
