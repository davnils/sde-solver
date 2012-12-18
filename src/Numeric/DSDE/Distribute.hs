{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns,
             ExistentialQuantification, FlexibleInstances, DeriveGeneric,
             DefaultSignatures, ScopedTypeVariables, TupleSections,
             FlexibleContexts #-}

module Numeric.DSDE.Distribute where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Identity hiding (mapM)
import Control.Monad.State
import Control.Parallel
import Control.Parallel.MPI.Simple 
import Data.Foldable (fold, foldl')
import Data.Monoid
import Data.Serialize (Serialize(..))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Serialize
import GHC.Generics (Generic)
import Numeric.DSDE.RNG
import Numeric.DSDE.SDE
import Numeric.DSDE.SDESolver
import Prelude hiding (sum, init, map)

-- | Wrapper used by all distributors supplied to the 'evaluate' function.
data DistributeInstance m = forall a. Distribute a m => Distr a

-- | Container describing the result produced by an SDE solution.
data SDEResult = Scalar !Double !Int             -- ^ Average of all end-point values, with the number of samples recorded.
               | Distribution !(V.Vector Double) -- ^ All end-point samples stored as an unboxed vector.
  deriving (Generic, Show)

-- | Monoid instance used when folding results from multiple sources.
instance Monoid SDEResult where
  mempty = Scalar 0 0
  Scalar a n         `mappend` Scalar b n'        =
    Scalar (elemSum / fromIntegral s) s
    where
    s = n + n'
    elemSum = a * fromIntegral n + b * fromIntegral n'

  Distribution v     `mappend` Distribution v'    = Distribution $ v V.++ v'
  Scalar _ _         `mappend` d@(Distribution _) = d
  d@(Distribution _) `mappend` Scalar _ _         = d

-- | Serialize instance used by MPI.
instance Serialize SDEResult

-- | Internal abstraction over the choice of specifying either the interval length or the number of steps.
data Accuracy = End Double
              | Steps Int
  deriving (Generic, Show)

-- | Serialize instance used by MPI.
instance Serialize Accuracy

-- | Set of parameters supplied to solve an SDE problem.
data InstanceParams = IP {
  accuracy :: !Accuracy,
  start :: !Double,
  deltat :: !Double,
  simulations :: !Int }
  deriving (Generic, Show)

-- | Serialize instance used by MPI.
instance Serialize InstanceParams

-- | MPI cluster distributor.
data MPI = MPI

-- | Local evaluation using GHC threads
data Local = Local Int

type SDEConstraint b c g m p = (SDE b, SDESolver c, Parameter p, RNGGen g m p)
type SDEInstance b c g m p = (b p, c, Maybe Int -> m g, InstanceParams)

-- | Type class indicating the ability to distribute data in some way.
--   Several distributors may be chained.
class Monad m => Distribute a m where
  -- | Inject an SDE instance into the context.
  inject :: SDEConstraint b c g m p =>
    a -> SDEInstance b c g m p -> m (SDEInstance b c g m p)
  -- | Remove an SDE result from the context.
  remove :: a -> SDEResult -> m SDEResult

-- | Type class indicating ability to solve an SDE problem.
class Execute a m p where
  execute :: SDEConstraint b c g m p =>  a -> SDEInstance b c g m p -> m SDEResult

-- | Type class indicating ability to perform a set of actions in an efficient way.
class Mappable m p where
  map' :: RNGGen g m p => (Int, Maybe Int -> m g) -> (g -> m b) -> [a] -> m [b] 

-- | Mappable instance for the IO monad. Work is divided using forkIO.
instance Mappable IO Double where
  map' (seed, rng) f l = do
    rand <- rng (Just seed)
    seeds <- mapM (\_ -> (Just . round <$> getRand rand) >>= rng) l
    mapM splitWork seeds >>= mapM takeMVar
    where
    splitWork rand = do
      var <- newEmptyMVar
      forkIO $ f rand >>= putMVar var
      return var

-- | Mappable instance for the pure State monad, uses 'par' annotations.
--   This does not perform well in general and needs to be optimized to
--   compete with the monadic IO instance.
instance RealFrac a => Mappable (State s) a where
  map' (seed, rng) f l = (go f seed l >>= sequence)
    where
    go f _ [] = return []
    go f s (_:t) = do
      worker <- rng (Just s)
      s' <- round <$> getRand worker
      rest <- go f s' t
      return $ f worker `par` f worker : rest

-- | Distribute instance over MPI which defines data transportation.
instance Distribute MPI IO where
  inject _ (sde, solver, rng, params) = do
    init
    size <- commSize commWorld
    rank <- commRank commWorld
    (sde, solver, rng,) <$> case rank of
      0 -> do
        let slaveSize = ceiling $ (fromIntegral $ simulations params :: Double) /
                                   fromIntegral size
        let slave = params { simulations = slaveSize}
        bcastSend commWorld 0 slave
        return slave

      _ -> bcastRecv commWorld 0

  remove _ localResult = do
    result <- commRank commWorld >>= retrieve
    finalize
    return result
    where
    retrieve 0 = do
      clusterResult <- gatherRecv commWorld 0 localResult
      return $ fold clusterResult
    retrieve _ =
      gatherSend commWorld 0 localResult >> return localResult

-- | Generic execute instances over any mappable monad 'm'.
instance Mappable m Double => Execute Local m Double where
  execute (Local cores) (!sde, !solver, !rng, params) = do
    seedRNG <- round <$> (rng Nothing >>= getRand)
    fold <$> map' (seedRNG, rng) runThread [1..cores]
    where
    perThread = ceiling $ (fromIntegral $ simulations params :: Double) /
                           fromIntegral cores :: Int
    steps = case accuracy params of
      End endTime -> floor $ endTime / deltat params
      Steps n -> n

    runThread rng = Distribution <$> thread rng
    thread rand = V.mapM (const $ threadEvaluation rand) $ V.replicate perThread (0.0 :: Double)
    threadEvaluation rand = foldM' (eval rand) (start params) [1..steps]
    eval rand w_i step = w_iplus1 solver sde rand (fromIntegral step * deltat params) w_i (deltat params)

-- | Evaluate the SDE using the supplied distributors and execution method.
evaluate :: (Monad m, SDEConstraint b c g m p, Execute e m p) =>
  ([DistributeInstance m], e) -> SDEInstance b c g m p -> m SDEResult
evaluate ([], method) input = execute method input
evaluate (Distr method : other, final) input =
  inject method input >>= evaluate (other, final) >>= remove method

-- | Monadic strict fold.
foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' func z (x:xs) = do
  z' <- func z x
  z' `seq` foldM' func z' xs
