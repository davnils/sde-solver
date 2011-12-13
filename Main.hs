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
        print $ runST (solve (bs :: BlackScholes) (initialize :: Int -> ST s (M.Gen s)) 4712)
