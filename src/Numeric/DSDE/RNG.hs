{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             TypeFamilies, IncoherentInstances, ConstraintKinds,
             FunctionalDependencies, BangPatterns #-}

module Numeric.DSDE.RNG where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.ST
import Control.Monad.State
import Data.Maybe
import qualified Data.Random.Normal as NORMAL
import qualified Data.Vector.Unboxed as V
import qualified System.Random.Mersenne.Pure64 as MT
import Numeric.DSDE.SDE (Parameter)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MD

defaultSeed :: Int
defaultSeed = 0

class (Monad m, Functor m, Parameter p) => RNGGen g m p | g m -> p where
  getRand :: g -> m p
  initialize :: Maybe Int -> m g

instance RNGGen (MWC.Gen s) (ST s) Double where
  getRand = MD.normal 0 1
  initialize s = MWC.initialize . V.singleton . fromIntegral $ fromMaybe defaultSeed s

instance (MWC.GenIO ~ d) => RNGGen d IO Double where
  {-# INLINE getRand #-}
  getRand = MD.normal 0 1
  {-# INLINE initialize  #-}
  initialize (Just n) = MWC.initialize . V.singleton . fromIntegral $ n
  initialize Nothing = MWC.withSystemRandom . MWC.asGenIO $ return

instance RNGGen MT.PureMT (State MT.PureMT) Double where
  {-# INLINE getRand #-}
  getRand _ = do
    !(sample, gen') <- NORMAL.normal <$> get
    put gen'
    return sample
  {-# INLINE initialize  #-}
  initialize = return . MT.pureMT . fromIntegral . fromMaybe defaultSeed
