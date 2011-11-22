module Main where

import Data.List (foldl')
import System.Random.MWC

type Rate = Double
type Volatility = Double

step :: (Rate, Volatility) -> Double -> Double -> Double -> Double
step (rate, volatility) stepSize prev rand = prev * inner
        where
                inner = foldl' (+) 0 [1, first, second, third]
                first = rate * stepSize
                second = volatility * dB
                third = 0.5 * volatility^2 * (dB^2 - stepSize)
                dB = rand * sqrt stepSize

evalValue :: Int -> (Rate, Volatility) -> (Double, Double) -> [Double] -> Double
evalValue n param (start, time) = foldl' (step param stepSize) start
        where stepSize = time / realToFrac n

main :: IO ()
main = do
        let n = 1000
        let simulations = 500

        rand <- genRandom (n*simulations)
        --print $ sum rand / realToFrac 1000000

        let rands = split n simulations rand
        let res = map (evalValue n (0.08, 0.25) (12, 2)) rands
        print $ sum res / realToFrac simulations

genRandom :: Int -> IO [Double]
genRandom num = withSystemRandom $ \gen -> do
        let limit = num-- / realToFrac 2
        mapM (const $ (normal :: GenIO -> IO Double) gen) [1..limit]
        --return concat $ map (\a -> [a, negate a]) prev

split :: Int -> Int -> [Double] -> [[Double]]
split _ 0 _ = []
split size n rand = first : split size (n - 1) sec
        where (first, sec) = splitAt size rand
