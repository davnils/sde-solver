{-# LANGUAGE BangPatterns #-}
module BlackScholes where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import RNG
import SDE

type Rate = Double
type Volatility = Double

data BlackScholes = BS !Rate !Volatility !Double !Double !TimeStep !Int

instance SDE BlackScholes where
        step (BS rate volatility _ stepSize _ _) rng !prev = do
                rand <- getRand rng
                let dB = rand * sqrt stepSize
                return $ prev * pure dB
                        where
                        pure dB = 1 + first + second dB + third dB
                        first = rate * stepSize
                        second = (*volatility)
                        third dB = 0.5 * volatility^2 * (dB^2 - stepSize)

        config (BS _ _ start stepSize endTime simulations) =
                (start, StepSize stepSize endTime, simulations)
        parseCmd [] = error "No arguments given"
        parseCmd l' = fromMaybe (parseCmd []) b
                where
                b = liftM6 BS (f "rate") (f "vol") (f "start")
                        (f "step") (f "end") (floor <$> f "simulations")
                -- TODO: Extend parsing of arguments and fix type workarounds

                f = flip lookup l
                l = map parse . filter (even . fst) . zip [0..] $ zip l' (tail l')
                parse = second read . snd
                -- TODO: Separate argument parsing for different purposes

liftM6 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
        -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m b
liftM6 f a1 a2 a3 a4 a5 a6 = return f `ap` a1 `ap` a2 `ap` a3 `ap` a4 `ap` a5 `ap` a6
