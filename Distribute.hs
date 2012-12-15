{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, BangPatterns,
             ExistentialQuantification, FlexibleInstances, DeriveGeneric,
             DefaultSignatures, ScopedTypeVariables, TupleSections,
             FlexibleContexts #-}

module Distribute where

import Control.Applicative ((<$>))
import Control.Monad.Identity hiding (mapM)
import qualified Control.Monad.Parallel as P
import Control.Parallel.MPI.Simple 
{- import qualified Data.Array.Accelerate as Acc
import qualified Data.Array.Accelerate.CUDA as CUDA -}
import Data.Foldable (fold, foldl')
import Data.Monoid
import Data.Serialize (Serialize(..))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Serialize
import GHC.Generics (Generic)
import Prelude hiding (sum, init)
import RNG
import SDE
import SDESolver

data DistributeInstance m = forall a. Distribute a m => Distr a

data SDEResult = Scalar !Double !Int
               | Distribution !(V.Vector Double)
  deriving (Generic, Show)

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
-- data GPUAccelerate = GPUAccelerate
data Local = Local Int

type SDEConstraint b c g m p = (SDE b, SDESolver c, Parameter p, RNGGen g m p)
type SDEInstance b c g m p = (b p, c, Maybe Int -> m g, InstanceParams)

class Monad m => Distribute a m where
  inject :: SDEConstraint b c g m p =>
    a -> SDEInstance b c g m p -> m (SDEInstance b c g m p)
  remove :: a -> SDEResult -> m SDEResult

class Execute a m p where
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

instance P.MonadParallel m => Execute Local m Double where
  execute (Local cores) (!sde, !solver, !rng, params) = 
    fold <$> P.mapM (const runThread) [1..cores]
    where
    perThread = ceiling $ (fromIntegral $ simulations params :: Double) /
                           fromIntegral cores :: Int
    steps = case accuracy params of
      End endTime -> floor $ endTime / deltat params
      Steps n -> n

    runThread = Distribution <$> (rng Nothing >>= thread)
    thread rand = V.mapM (const $ threadEvaluation rand) $ V.replicate perThread (0.0 :: Double)
    threadEvaluation rand = foldM' (eval rand) (start params) [1..steps]
    eval rand w_i _ = w_iplus1 solver sde rand 0 w_i (deltat params)

average :: Fractional a => [a] -> a
average l = foldl' (+) 0 l / realToFrac (length l)

{- instance Execute GPUAccelerate Identity (Acc.Exp Float) where
  execute _ (!sde, !solver, !rng, params) = return $ (`Scalar` (simulations params)) . float2Double .  head . Acc.toList $ CUDA.run $ result
    where
    steps = case accuracy params of
      End endTime -> floor $ endTime / deltat params
      Steps n -> n

    deltat' = Acc.constant $ deltat params
    start' = Acc.constant $ start params
    simulations' = Acc.constant $ 10 --simulations params
    steps' = steps

    seeds = Acc.enumFromN (Acc.index1 simulations') (0 :: Acc.Exp Float)
    --flattened = Acc.enumFromN (Acc.index1 . Acc.constant $ (simulations params) * steps) 0
    --segs = Acc.fill (Acc.index1 $ simulations') steps'

    average' l = Acc.fold (\n acc -> acc + Acc.constant 5.0 {- (n/(Acc.fromIntegral $ Acc.size l))-}) (Acc.constant 0.0) l

    -- easier to `fold like a fool`?
    -- express computations a single large matrix:
    -- r01 r02 r03 r04 r05 r06 r07 r08 r09
    -- r11 r12 r13 r14 r15 r16 r17 r18 r19
    -- each row describes a realisation, yeilding a matrix of dimension simulations x steps
    -- the elements should be the random numbers drawn from N(0,1), but will be constant for now
    -- the computation is done by folding each row with the underlying expression, 
    -- and the resulting column vector with averaging.
    --
    -- structure:
    -- flatten matrix into a single vector
    -- perform segmented reduction with segment lengths of `steps` and reduce the result
    realisations = seeds --Acc.foldSeg eval start' flattened segs

    --eval w_i _ = runIdentity $ rng Nothing >>= \num -> w_iplus1 solver sde num 0 w_i deltat'
    result = average' realisations -}

evaluate :: (Monad m, SDEConstraint b c g m p, Execute e m p) =>
  ([DistributeInstance m], e) -> SDEInstance b c g m p -> m SDEResult
evaluate ([], method) input = execute method input
evaluate (Distr method : other, final) input =
  inject method input >>= evaluate (other, final) >>= remove method

foldM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' func z (x:xs) = do
  z' <- func z x
  z' `seq` foldM' func z' xs
