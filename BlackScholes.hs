module BlackScholes where

import Control.Arrow
import Control.Monad
import Data.Maybe
import RNG
import SDE

type Rate = Double
type Volatility = Double

data BlackScholes = BS Rate Volatility Double Double TimeStep

instance SDE BlackScholes where
	step (BS rate volatility _ stepSize _) rng prev = do 
		rand <- getRand rng
		let dB = rand * sqrt stepSize
		return $ prev * pure dB
			where
			pure dB = 1 + first + second dB + third dB
			first = rate * stepSize
			second = (*volatility)
			third dB = 0.5 * volatility^2 * (dB^2 - stepSize)

	config (BS _ _ start stepSize endTime) =
		(start, StepSize stepSize endTime)
	parseCmd [] = error "No arguments given"
	parseCmd l' = fromMaybe (parseCmd []) b
		where
		b = liftM5 BS (f "rate") (f "vol") (f "start") (f "step") (f "end")
		f = flip lookup l :: String -> Maybe Double
		l = map parse . filter (even . fst) . zip [0..] $ zip l' (tail l')
		parse = (second read) . snd
