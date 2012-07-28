{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             TypeFamilies, IncoherentInstances #-}

module RNG where

import Control.Applicative
import Control.Monad.ST
import Control.Monad.State
import Data.Maybe
--import Data.Random.Normal
import qualified Data.Vector.Unboxed as V
import qualified System.Random as R
import qualified System.Random.MWC as M
import qualified System.Random.MWC.Distributions as MD

defaultSeed :: Int
defaultSeed = 0

class (Monad m, Functor m) => RNGGen g m where
  getRand :: g -> m Double
  initialize :: Maybe Int -> m g

instance RNGGen (M.Gen s) (ST s) where
  getRand = MD.normal 0 1
  initialize = M.initialize . V.singleton . fromIntegral . fromMaybe defaultSeed

instance (M.GenIO ~ d) => RNGGen d IO where
  {-# INLINE getRand #-}
  getRand = MD.normal 0 1
  {-# INLINE initialize  #-}
  initialize (Just n) = M.initialize . V.singleton . fromIntegral $ n
  initialize Nothing = M.withSystemRandom . M.asGenIO $ return

data PrimitiveGen = PG Int | StdPG R.StdGen
  deriving Show

instance RNGGen PrimitiveGen IO where
  getRand (PG gen) = return $ realToFrac gen + 1 -- TODO Normal-bypass
  initialize = return . PG . fromMaybe defaultSeed

instance RNGGen PrimitiveGen (State PrimitiveGen) where
  getRand _ = do
    (sample, gen') <- normal . (\(StdPG g) -> g) <$> get
    put $ StdPG gen'
    return sample
  initialize = return . StdPG . R.mkStdGen . fromMaybe defaultSeed

normal = undefined
