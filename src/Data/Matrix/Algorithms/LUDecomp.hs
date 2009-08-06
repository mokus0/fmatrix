{-# LANGUAGE ScopedTypeVariables #-}
module Data.Matrix.Algorithms.LUDecomp where

import Data.Matrix.Math
import Data.Matrix.Algorithms.Substitution

import Control.Monad
import Control.Monad.ST
import Data.Permute
import Data.Permute.ST
import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.StateRef
import Data.Array.MArray (MArray)
import Data.List
import Data.Ord
import Data.Complex

luImproveM a l u indx b x0 = x0 `subM` dx
    where
        dx = luSolveM l u indx rhs
        rhs = a `multiply` x0 `subM` b
        subM = liftMatrix2 (-)

luInv a = luSolveM l u indx b
    where
        (l,u,indx,_) = ludcmp a
        n = matRows a
        b = kronecker n `asTypeOf` a

det :: (Fractional a, Ord a, Matrix m a) => m a -> a
det a = case ludcmp a of
    (l,u,indx,d) -> sign (reduceDiagonal product u)
        where
            sign | d         = id
                 | otherwise = negate

ludcmp :: (Fractional a, Ord a, Matrix m a) =>
          m a -> (FunctionMatrix a, FunctionMatrix a, Permute, Bool)
ludcmp a = runST (ludcmp_st a)

ludcmp_complex a = runST (ludcmp_complex_st a)

luSolveV l u indx = backSub  u . forwardSub  l . permuteV indx
luSolveM l u indx = backSubs u . forwardSubs l . permuteRows indx

ludcmp_st a = do
    (lu, indx, d) <- ludcmp_generic (comparing abs) a
    lu <- (unsafeFreezeMatrix :: STMatrix s t -> ST s (IMatrix t)) lu
    let (l,u) = lu_split lu indx
    return (l,u,indx,d)

ludcmp_complex_st a = do
    (lu, indx, d) <- ludcmp_generic (comparing magnitude) a
    lu <- (unsafeFreezeMatrix :: STMatrix s t -> ST s (IMatrix t)) lu
    let (l,u) = lu_split lu indx
    return (l,u,indx,d)

ludcmp_generic cmp a = do
    let n = matRows a
    indx <- newPermute n
    lu <- copyMatrix a
    d  <- newDefaultRef True    -- permutation parity: True = even
    
    vv <- rowReduceM lu $ \xs -> do
        let x = maximumBy cmp xs
        when (x == 0) $ fail "ludcmp: Singular Matrix"
        return (recip x)
    
    sequence_
        [ do
            big <- newDefaultRef 0 
            imax <- newDefaultRef k
            
            -- find pivot
            sequence_
                [ do
                    i' <- getElem indx i
                    
                    v <- readV vv i'
                    x <- readM lu i' k
                    let temp = v * x
                    big_ <- readRef big
                    when (temp `cmp` big_ == GT) $ do
                        writeRef big temp
                        writeRef imax i
                | i <- [k..n-1]
                ]
            
            -- execute pivot
            imax <- readRef imax
            when (k /= imax) $ do
                modifyRef d not
                swapElems indx k imax
            
            k' <- getElem indx k
            piv <- readM lu k' k
            when (piv == 0) $ fail "ludcmp: Singular Matrix"
            let pivInv = recip piv
            
            sequence_
                [ do
                    i' <- getElem indx i
                    t <- readM lu i' k
                    let temp = t * pivInv
                    writeM lu i' k temp
                    
                    sequence_
                        [ do
                            t <- readM lu i' j
                            lu_k_j <- readM lu k' j
                            writeM lu i' j (t - temp * lu_k_j)
                        | j <- [k+1..n-1]
                        ]
                | i <- [k+1..n-1]
                ]
            
            
        | k <- [0..n-1]
        ]
    
    indx <- unsafeFreeze indx
    d <- readRef d
    
    return (lu,indx,d)

lu_split :: (Matrix m t, Num t) => m t -> Permute -> (FunctionMatrix t, FunctionMatrix t)
lu_split lu indx = (l,u)
    where
        n = matRows lu
        l = matrix n n $ \i j -> case i `compare` j of
            LT -> 0
            EQ -> 1
            GT -> indexM lu (indx `at` i) j
        u = matrix n n $ \i j -> if i > j
            then 0
            else indexM lu (indx `at` i) j


-- rowReduceST :: STMatrix s a -> ([a] -> ST s b) -> ST s (STVector s b)
-- rowReduceM :: (MMatrix mat a m, MVector v b m) =>
--               mat a -> ([a] -> m b) -> m (v b)
rowReduceM :: (MArray a t1 m, MArray a t2 m) =>
              ArrayMatrix a t1 -> ([t1] -> m t2) -> m (ArrayVector a t2)
rowReduceM m f = do
    (r,c) <- getMatSize m
    vs <- sequence
        [ do
            xs <- mapM (readM m i) [0..c-1]
            f xs
        | i <- [0..r-1]
        ]
    newVector r (vs!!)

reduceDiagonal f m = f [indexM m i i | i <- [0..min (matRows m) (matCols m) - 1]]