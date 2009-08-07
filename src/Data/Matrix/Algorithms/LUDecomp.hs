{-# LANGUAGE ScopedTypeVariables #-}
module Data.Matrix.Algorithms.LUDecomp where

import Data.Matrix.Math
import Data.Matrix.Alias
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

a, b, x' :: UMatrix Double -- can be any Matrix instance
a  = matrixFromList [[1,3,-2],[3,5,6],[2,4,3]]
b  = matrixFromList [[5],[7],[8]] 
x' = matrixFromList [[-15],[8],[2]]
x :: IMatrix Double
x  = luSolveM l u indx b
(l,u,indx,d) = ludcmp a


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

ludcmp_st :: (Ord t, Fractional t, Matrix m t) =>
             m t -> ST s (FunctionMatrix t, FunctionMatrix t, Permute, Bool)
ludcmp_st a = do
    lu <- copyMatrix a
    (indx, d) <- ludcmp_generic (comparing abs) lu
    lu <- (unsafeFreezeMatrix :: STMatrix s t -> ST s (IMatrix t)) lu
    let (l,u) = lu_split lu indx
    return (l,u,indx,d)

ludcmp_complex_st a = do
    lu <- copyMatrix a
    (indx, d) <- ludcmp_generic (comparing magnitude) lu
    lu <- (unsafeFreezeMatrix :: STMatrix s t -> ST s (IMatrix t)) lu
    let (l,u) = lu_split lu indx
    return (l,u,indx,d)

ludcmp_generic cmp luRaw = do
    n <- getNumRows luRaw
    
    vvRaw <- rowReduceM luRaw $ \xs -> do
        -- compute scaling factors for each row for use in implicit pivoting
        let x = maximumBy cmp xs
        when (x == 0) $ fail "ludcmp: Singular Matrix"
        return (recip x)
        
    indx <- newPermute n
    d  <- newDefaultRef True    -- permutation parity: True = even
    
    -- lu and vv are permuted versions of luRaw and vvRaw.
    -- The permutation is "live": whenever indx changes, so do these.
    lu <- aliasMatrixWith (RowMPermute indx) luRaw
    vv <- aliasVectorWith (VecMPermute indx) vvRaw
    
    sequence_
        [ do
            pivot cmp indx d lu vv n k
            
            piv <- readM lu k k
            when (piv == 0) $ fail "ludcmp: Singular Matrix"
            let pivInv = recip piv
            
            sequence_
                [ do
                    temp <- modifyM lu i k (*pivInv)
                    
                    sequence_
                        [ updateM lu i j $ \t -> do
                            lu_k_j <- readM lu k j
                            return (t - temp * lu_k_j)
                        | j <- [k+1..n-1]
                        ]
                | i <- [k+1..n-1]
                ]
        | k <- [0..n-1]
        ]
    
    indx <- unsafeFreeze indx
    d <- readRef d
    
    return (indx,d)

pivot cmp indx d lu vv n k = do
    imax <- selectPivot cmp lu vv n k
    
    -- execute pivot
    when (k /= imax) $ do
        modifyRef d not
        swapElems indx k imax -- also swaps rows in lu and elems in vv

selectPivot cmp lu vv n k = go k k 0
    where
        go i imax big
            | i >= n    = return imax
            | otherwise = do
                v <- readV vv i
                x <- readM lu i k
                let temp = v * x
                case temp `cmp` big of
                    GT -> go (i+1) i    temp
                    _  -> go (i+1) imax big

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