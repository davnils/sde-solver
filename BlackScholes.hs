module Main where

import Data.List (foldl')

type Prime = Int
type Rate = Double
type Volatility = Double

-- How should input data be garthered?
-- either:
-- (1) coded into the instance implementation
-- (2) parsed from command line (maybe like a setter also?)
class SDE a where
	step' :: a -> m Double
	initial :: a -> m Double
	parseCmd :: String -> m a

step :: (Rate, Volatility) -> Double -> Double -> Double -> Double
step (rate, volatility) stepSize prev rand = prev * inner
        where
                inner = 1 + first + second + third
                first = rate * stepSize
                second = volatility * dB
                third = 0.5 * volatility^2 * (dB^2 - stepSize)
                dB = rand * sqrt stepSize

evalValue :: Int -> (Rate, Volatility) -> (Double, Double) -> [Prime] -> Int -> Double
evalValue n param (start, time) bases index = fst folded
        where
        folded = foldl' foldStep (start, 0) stateList
        stateList = zip (zip bases $ tail (bases ++ [0])) [1..n]
        stepSize = time / realToFrac n

        foldStep :: (Double, Double) -> ((Prime, Prime), Int) -> (Double, Double)
        foldStep (y, rand) ((b1, b2), i)
                | odd i = (step param stepSize y r1, r2)
                | otherwise = (step param stepSize y rand, 0.0)
                where
                (r1, r2) = boxMuller (q b1) (q b2)
                q = genHalton index

boxMuller :: Double -> Double -> (Double, Double)
boxMuller u1 u2 = (z1, z2)
        where
        z1 = root * cos(2*pi*u2)
        z2 = root * sin(2*pi*u2)
        root = sqrt (-2 * log u1)

genHalton :: Int -> Prime -> Double
genHalton index base = haltonStep 0 f index
        where
        f = 1 / realToFrac base
        haltonStep :: Double -> Double -> Int -> Double
        haltonStep sum f i
                | i <= 0 = sum
                | otherwise = haltonStep sum' f' i'
                        
                where
                sum' = sum + f * (realToFrac $ mod i base)
                f' = f / realToFrac base
                i' = floor $ realToFrac i / realToFrac base

primes :: [Prime]
primes = [x | x <- [23,25..], isPrime x]
        where isPrime x = all (\y -> mod x y /= 0) [2.. x-1]

main :: IO ()
main = do
        let n = 500
        let simulations = 1000

        --let rands = split n simulations [1..]
        let bases = take n (drop 100 primes)
        --print $ sum bases
        let res = map (evalValue n (0.08, 0.25) (15, 1) bases) [1..simulations]
        print $ sum res / realToFrac simulations

split :: Int -> Int -> [Double] -> [[Double]]
split _ 0 _ = []
split size n rand = first : split size (n - 1) sec
        where (first, sec) = splitAt size rand
