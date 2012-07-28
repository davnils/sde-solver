{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns,
             ExistentialQuantification, FlexibleInstances, DeriveGeneric,
             DefaultSignatures, ScopedTypeVariables, TupleSections #-}
module Distribute where

-- TODO: (1) Fix t_i
--       (2) Check if the required bottom-execution can be encoded in the type system

import Control.Applicative ((<$>))
import Control.Monad.Identity hiding (mapM)
import qualified Control.Monad.Parallel as P
import Control.Parallel.MPI.Simple 
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

data Accuracy = End TimeStep
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

type SDEConstraint b c g m = (SDE b, SDESolver c, RNGGen g m)
type SDEInstance b c g m = (b, c, Maybe Int -> m g, InstanceParams)

class (P.MonadParallel m) => Distribute a m where
  inject :: SDEConstraint b c g m => a -> SDEInstance b c g m -> m (SDEInstance b c g m)
  execute :: SDEConstraint b c g m =>  a -> SDEInstance b c g m -> m SDEResult
  remove :: a -> SDEResult -> m SDEResult

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

  execute _ _ = return NotApplicable

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

instance (P.MonadParallel m) => Distribute Local m where
  inject _ = id <$> return
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

  remove _ = id <$> return

evaluate :: (P.MonadParallel m, SDEConstraint b c g m) => [DistributeInstance m] ->
  SDEInstance b c g m -> m SDEResult
evaluate [] _ = return NotApplicable
evaluate (Distr method : []) input =
  inject method input >>= execute method >>= remove method
evaluate (Distr method : other) input =
  inject method input >>= evaluate other >>= remove method

foldM' :: (P.MonadParallel m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' func z (x:xs) = do
  z' <- func z x
  z' `seq` foldM' func z' xs
