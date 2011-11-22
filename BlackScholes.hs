module BlackScholes where

import Control.Applicative
import Data.Random.Normal (normalsIO')

type Rate = Double
type Volatility = Double

step :: Rate -> Volatility -> Double -> Double -> Double -> Double
step rate volatility stepSize prev rand = prev * inner
	where
		inner = foldl (+) 0 [1, first, second, third]
		first = rate*stepSize
		second = volatility*dB
		third = 0.5*volatility^2*(dB^2 - stepSize)
		dB = rand * (sqrt stepSize)
		
main :: IO ()
main = take n . scanl (step 0.08 0.25 0.01) 12 <$> normalsIO' (0, 1) >>= print
	where n = truncate $ 2.0 / 0.01
