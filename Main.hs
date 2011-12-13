module Main where

import BlackScholes
import Control.Applicative
import Control.Monad.ST
import RNG
import SDE
import System.Environment
import System.IO
import qualified System.Random.MWC as M

main :: IO ()
main = do
        bs <- parseCmd <$> getArgs
        print $ runST (solve bs)

solve :: BlackScholes -> ST s Double
solve bs = do
	g <- initialize 4712 :: ST s (M.Gen s)
	evalValue bs g
