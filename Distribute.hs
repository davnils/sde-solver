{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns,
             ExistentialQuantification, FlexibleInstances, DeriveGeneric,
             DefaultSignatures, ScopedTypeVariables, TupleSections #-}

module Distribute where

-- TODO: (1) Fix t_i

import Control.Applicative ((<$>))
import Control.Monad.Identity hiding (mapM)
import qualified Control.Monad.Parallel as P
import Control.Parallel.MPI.Simple 
import qualified Data.Array.Accelerate as Accelerate
import Data.Foldable (fold, foldl')
import Data.Monoid
import Data.Serialize (Serialize(..))
import GHC.Generics (Generic)
import Prelude hiding (sum, init)
import RNG
import SDE
import SDESolver

data DistributeInstance m = forall a. Distribute a m => Distr a

data SDEResult = Scalar !Double !Int
               | NotApplicable
  deriving (Generic, Show)

instance Monoid SDEResult where
  mempty = Scalar 0 0
  Scalar a n `mappend` Scalar b n' =
    Scalar (elemSum / fromIntegral s) s
      where
      s = n + n'
      elemSum = a * fromIntegral n + b * fromIntegral n'
  _ `mappend` _ = NotApplicable

instance Serialize SDEResult

data Accuracy = End Double
              | Steps Int
  deriving (Generic, Show)

instance Serialize Accuracy

data InstanceParams = IP {
  accuracy :: !Accuracy,
  start :: !Double,
  deltat :: !Double,
  simulations :: !Int }
  deriving (Generic, Show)

instance Serialize InstanceParams

data MPI = MPI
data GPUAccelerate = GPUAccelerate
data Local = Local Int

type SDEConstraint b c g m p = (SDE b, SDESolver c, Parameter p, RNGGen g m p)
type SDEInstance b c g m p = (b p, c, Maybe Int -> m g, InstanceParams)

class (P.MonadParallel m) => Distribute a m where
  inject :: SDEConstraint b c g m p =>
    a -> SDEInstance b c g m p -> m (SDEInstance b c g m p)
  remove :: a -> SDEResult -> m SDEResult

class (P.MonadParallel m) => Execute a m p where
  execute :: SDEConstraint b c g m p =>  a -> SDEInstance b c g m p -> m SDEResult

-- TODO: action size rank `finally` finalize
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

instance (P.MonadParallel m) => Execute Local m Double where
  execute (Local cores) (!sde, !solver, !rng, params) = 
    fold <$> P.mapM (const runThread) [1..cores]
    where
      perThread = ceiling $ (fromIntegral $ simulations params :: Double) /
                             fromIntegral cores :: Int
      steps = case accuracy params of
        End endTime -> floor $ endTime / deltat params
        Steps n -> n

      runThread = (`Scalar` perThread) . average <$> (rng Nothing >>= thread)
      thread rand = mapM (const $ threadEvaluation rand) [1..perThread]
      threadEvaluation rand = foldM' (eval rand) (start params) [1..steps]
      eval rand w_i _ = w_iplus1 solver sde rand 0 w_i (deltat params)
      average l = foldl' (+) 0 l / realToFrac (length l)

instance Execute GPUAccelerate IO (Accelerate.Exp Double) where
  execute _ _ = undefined

evaluate :: (P.MonadParallel m, SDEConstraint b c g m p, Execute e m p) =>
  ([DistributeInstance m], e) -> SDEInstance b c g m p -> m SDEResult
evaluate ([], method) input = execute method input
evaluate (Distr method : other, final) input =
  inject method input >>= evaluate (other, final) >>= remove method

foldM' :: (P.MonadParallel m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' func z (x:xs) = do
  z' <- func z x
  z' `seq` foldM' func z' xs
