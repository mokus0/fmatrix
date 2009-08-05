{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
module Data.Matrix.Mutable where

import Data.Matrix.Types

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST

-- TODO: generalize...

newtype ArrayMatrix a t = ArrayMatrix { unArrayMatrix :: (a (Int, Int) t) }

instance (IArray a t) => Matrix (ArrayMatrix a) t
    where
        matRows (ArrayMatrix m) = case bounds m of
            ((0,_), (r,_)) -> r+1
        matCols (ArrayMatrix m) = case bounds m of
            ((_,0), (_,c)) -> c+1
        matrix r c m = ArrayMatrix $ listArray ((0,0), (r-1,c-1))
            [ m i j
            | i <- [0..r-1]
            , j <- [0..c-1]
            ]
        unsafeIndexM (ArrayMatrix m) r c = m ! (r,c)

instance (IArray a t, Show t) => Show (ArrayMatrix a t) where
    showsPrec p = showsMatrix

type IMatrix = ArrayMatrix Array
type UMatrix = ArrayMatrix UArray
type STMatrix s = ArrayMatrix (STArray s)
type STUMatrix s = ArrayMatrix (STUArray s)

copyMatrix :: (Matrix m t, MArray a t f) => m t -> f (ArrayMatrix a t)
copyMatrix m = do
    let r = matRows m
        c = matCols m
    m <- newListArray ((0,0), (r-1,c-1))
        [ indexM m i j
        | i <- [0..r-1]
        , j <- [0..c-1]
        ]
    return (ArrayMatrix m)

runSTMatrix :: (forall s. ST s (STMatrix s t)) -> IMatrix t
runSTMatrix m = ArrayMatrix (runSTArray (fmap unArrayMatrix m))

runSTUMatrix :: (forall s. ST s (STUMatrix s t)) -> UMatrix t
runSTUMatrix m = ArrayMatrix (runSTUArray (fmap unArrayMatrix m))

unsafeFreezeMatrix (ArrayMatrix m) = do
    m <- unsafeFreeze m
    return (ArrayMatrix m)


readM (ArrayMatrix m) i j = readArray m (i,j)
writeM (ArrayMatrix m) i j x = writeArray m (i,j) x

getNumRows (ArrayMatrix m) = do
    ((0,_),(r,_)) <- getBounds m
    return $! (r+1)
getNumCols (ArrayMatrix m) = do
    ((_,0),(_,c)) <- getBounds m
    return $! (c+1)

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

