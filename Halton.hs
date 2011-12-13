module Halton where

type Prime = Int
data HaltonGen = HG [Prime]

instance RandomGen HaltonGen where
        getRand gen = undefined
        initialize n = return $ HG $ take n primes

primes :: [Prime]
primes = [x | x <- [23,25..], isPrime x]
        where isPrime x = all (\y -> mod x y /= 0) [2.. x-1]

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
                sum' = sum + f * realToFrac (mod i base)
                f' = f / realToFrac base
                i' = floor $ realToFrac i / realToFrac base

{-evalValue :: Int -> (Rate, Volatility) -> (Double, Double) -> [Prime] -> Int -> Double
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
                q = genHalton index-}

