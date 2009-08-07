{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Data.Matrix.Mutable where

import Data.Matrix.Types

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Data.Permute
import Data.Permute.MPermute hiding (unsafeFreeze, unsafeThaw)

class Monad m => MMatrix mat t m where
    newMatrix :: Int -> Int -> (Int -> Int -> t) -> m (mat t)
    readM :: mat t -> Int -> Int -> m t
    writeM :: mat t -> Int -> Int -> t -> m ()
    modifyM :: mat t -> Int -> Int -> (t -> t) -> m t
    modifyM m i j f = do
        x <- readM m i j
        let fx = f x
        writeM m i j fx
        return fx
    updateM :: mat t -> Int -> Int -> (t -> m t) -> m t
    updateM m i j f = do
        x <- readM m i j
        x <- f x
        writeM m i j x
        return x
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

runSTVector :: (forall s. ST s (STVector s t)) -> IVector t
runSTVector v = ArrayVector (runSTArray (fmap unArrayVector v))

runSTUMatrix :: (forall s. ST s (STUMatrix s t)) -> UMatrix t
runSTUMatrix m = ArrayMatrix (runSTUArray (fmap unArrayMatrix m))

unsafeFreezeMatrix (ArrayMatrix m) = do
    m <- unsafeFreeze m
    return (ArrayMatrix m)

unsafeThawMatrix (ArrayMatrix m) = do
    m <- unsafeThaw m
    return (ArrayMatrix m)



unsafeFreezeVector (ArrayVector m) = do
    m <- unsafeFreeze m
    return (ArrayVector m)

unsafeThawVector (ArrayVector m) = do
    m <- unsafeThaw m
    return (ArrayVector m)


getNumRows m = do
    (r,c) <- getMatSize m
    return r
getNumCols m = do
    (r,c) <- getMatSize m
    return c

swapRowsM a r1 r2 = do
    n <- getNumCols a
    sequence_
        [ do
            r1v <- readM a r1 i
            r2v <- readM a r2 i
            writeM       a r1 i r2v
            writeM       a r2 i r1v
        
        | i <- [0..n-1]
        ]

permuteRowsM m p = sequence_
    [ swapRowsM m r1 r2
    | (r1, r2) <- swaps p
    ]
    
invPermuteColsM indx a = do
    swaps <- getInvSwaps indx
    sequence_
        [ swapColsM a r c
        | (r, c) <- swaps
        , r /= c
        ]

swapColsM a c1 c2 = do
    n <- getNumRows a
    sequence_
        [ do
            c1v <- readM a i c1
            c2v <- readM a i c2
            writeM a i c1 c2v
            writeM a i c2 c1v
        
        | i <- [0..n-1]
        ]

scaleRowM     a r x = mapRowM     a r (* x)
scaleRowM_n n a r x = mapRowM_n n a r (* x)

mapRowM a r f = do
    n <- getNumCols a
    mapRowM_n n a r f

mapRowM_n n a r f = sequence_
    [ do
        v <- readM a r i
        writeM a r i (f v)
    | i <- [0..n-1]
    ]

class Monad m => MVector v t m where
    newVector_ :: Int -> m (v t)
    newVector  :: Int -> (Int -> t) -> m (v t)
    getVecSize :: v t -> m Int
    readV  :: v t -> Int -> m t
    writeV :: v t -> Int -> t -> m ()
    modifyV :: v t -> Int -> (t -> t) -> m t
    modifyV v i f = do
        x <- readV v i
        let fx = f x
        writeV v i fx
        return fx


type STVector s = ArrayVector (STArray s)

instance (MArray a t m) => MVector (ArrayVector a) t m where
    newVector_ n = do
        v <- newArray_ (0,n-1)
        return (ArrayVector v)
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

swapVectors v1 v2 = do
    n1 <- getVecSize v1
    n2 <- getVecSize v2
    if n1 /= n2
        then fail "swapVectors: vectors' sizes differ"
        else unsafeSwapVectors n1 v1 v2

{-# INLINE unsafeSwapVectors #-}
unsafeSwapVectors n v1 v2 = sequence_
    [ do
        x1 <- readV v1 i
        x2 <- readV v2 i
        writeV       v1 i x2
        writeV       v2 i x1
    
    | i <- [0..n-1]
    ]

-- |zipWithV_is is vDest f v1 v2:
-- Like zipWith, but operates on vectors.  Operates on indices 'is' and 
-- places results in vDest, which may be the same as v1 or v2.
zipWithV_is is vDest f v1 v2  = sequence_
    [ do
        x <- readV v1 i
        y <- readV v2 i
        writeV vDest i (f x y)
    | i <- is
    ]

zipWithV_n n  = zipWithV_is [0..n-1]
zipWithV vDest f v1 v2 = do
    n1 <- getVecSize v1
    n2 <- getVecSize v2
    nD <- getVecSize vDest
    zipWithV_n (minimum [n1, n2, nD]) vDest f v1 v2
