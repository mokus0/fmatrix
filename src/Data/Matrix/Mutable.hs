{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Matrix.Mutable where

import Data.Matrix.Types

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST

class Monad m => MMatrix mat t m where
    newMatrix :: Int -> Int -> (Int -> Int -> t) -> m (mat t)
    readM :: mat t -> Int -> Int -> m t
    writeM :: mat t -> Int -> Int -> t -> m ()
    getMatSize :: mat t -> m (Int, Int)

type STMatrix s = ArrayMatrix (STArray s)
type STUMatrix s = ArrayMatrix (STUArray s)

instance (MArray a t m) => MMatrix (ArrayMatrix a) t m where
    newMatrix r c f = do
        m <- newListArray ((0,0), (r-1,c-1))
            [ f i j
            | i <- [0..r-1]
            , j <- [0..c-1]
            ]
        return (ArrayMatrix m)
    readM (ArrayMatrix m) i j = readArray m (i,j)
    writeM (ArrayMatrix m) i j x = writeArray m (i,j) x
    getMatSize (ArrayMatrix m) = do
        ((0,0),(r,c)) <- getBounds m
        return $! (r+1, c+1)

copyMatrix m = newMatrix (matRows m) (matCols m) (indexM m)

runSTMatrix :: (forall s. ST s (STMatrix s t)) -> IMatrix t
runSTMatrix m = ArrayMatrix (runSTArray (fmap unArrayMatrix m))

runSTUMatrix :: (forall s. ST s (STUMatrix s t)) -> UMatrix t
runSTUMatrix m = ArrayMatrix (runSTUArray (fmap unArrayMatrix m))

unsafeFreezeMatrix (ArrayMatrix m) = do
    m <- unsafeFreeze m
    return (ArrayMatrix m)


getNumRows m = do
    (r,c) <- getMatSize m
    return r
getNumCols m = do
    (r,c) <- getMatSize m
    return c

swapRows a r1 r2 = do
    n <- getNumCols a
    sequence_
        [ do
            r1v <- readM a r1 i
            r2v <- readM a r2 i
            writeM       a r1 i r2v
            writeM       a r2 i r1v
        
        | i <- [0..n-1]
        ]

swapCols a c1 c2 = do
    n <- getNumRows a
    sequence_
        [ do
            c1v <- readM a i c1
            c2v <- readM a i c2
            writeM a i c1 c2v
            writeM a i c2 c1v
        
        | i <- [0..n-1]
        ]

mapRowM a r f = do
    n <- getNumCols a
    sequence_
        [ do
            v <- readM a r i
            writeM a r i (f v)
        | i <- [0..n-1]
        ]

class MVector v t m where
    newVector :: Int -> (Int -> t) -> m (v t)
    getVecSize :: v t -> m Int
    readV  :: v t -> Int -> m t
    writeV :: v t -> Int -> t -> m ()


type STVector s = ArrayVector (STArray s)

instance (MArray a t m) => MVector (ArrayVector a) t m where
    newVector n f = do
        v <- newListArray (0,n-1)
            [ f i
            | i <- [0..n-1]
            ]
        return (ArrayVector v)
    getVecSize (ArrayVector v) = do
        (0,n) <- getBounds v
        return $! (n+1)
        
    readV  (ArrayVector v) = readArray v
    writeV (ArrayVector v) = writeArray v

copyVector v = newVector (vecElems v) (indexV v)

swapVecElems v i j = do
    iv <- readV v i
    jv <- readV v j
    writeV      v j iv
    writeV      v i jv
