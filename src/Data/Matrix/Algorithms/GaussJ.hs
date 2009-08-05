{-# LANGUAGE FlexibleContexts #-}
{-
 - Gauss-Jordan elimination with full pivoting, translated from C++
 - code given in Numerical Recipes, 3rd ed. (p. 44)
 -}
module Data.Matrix.Algorithms.GaussJ where

import Data.Matrix.Types
import Data.Matrix.Mutable

import Data.Array.ST

import Control.Monad
import Control.Monad.ST
import Data.StateRef
import Data.Permute.ST

import qualified Data.IntSet as S

-- examples:
-- a, b, x' :: UMatrix Double -- can be any Matrix instance
-- a  = matrixFromList [[1,3,-2],[3,5,6],[2,4,3]]
-- b  = matrixFromList [[5],[7],[8]] 
-- x' = matrixFromList [[-15],[8],[2]]
-- x :: UMatrix Double
-- x  = runSTUMatrix  $ do {a <- copyMatrix a; b <- copyMatrix b; gaussj_stu a b; return b}
-- aInv :: IMatrix Rational
-- aInv = gaussj_inv (convertByM toRational a :: IMatrix Rational)

gaussj :: (Fractional t,
           Ord t,
           Matrix m1 t,
           Matrix m2 t) =>
          m1 t -> m2 t -> (IMatrix t, IMatrix t)
gaussj a b = runST $ do
    a <- copyMatrix a 
    b <- copyMatrix b
    gaussj_st a b
    a <- unsafeFreezeMatrix a
    b <- unsafeFreezeMatrix b
    return (a,b)
     

gaussj_inv :: (Fractional t, Ord t, Matrix m t) => m t -> IMatrix t
gaussj_inv a = runSTMatrix $ do
    a <- copyMatrix a
    gaussj_inv_st a

gaussj_inv_st :: (Ord t, Fractional t) => STMatrix s t -> ST s (STMatrix s t)
gaussj_inv_st = gaussj_inv_generic

gaussj_inv_stu :: (Ord t, Fractional t, MArray (STUArray s) t (ST s)) 
    => STUMatrix s t -> ST s (STUMatrix s t)
gaussj_inv_stu = gaussj_inv_generic

gaussj_inv_generic a = do
    n <- getNumRows a
    b <- copyMatrix (FunctionMatrix n 0 undefined)
    gaussj_generic a (b `asTypeOf` a)
    return a



gaussj_st :: (Ord t, Fractional t) => STMatrix s t -> STMatrix s t -> ST s ()
gaussj_st = gaussj_generic

gaussj_stu :: (Ord t, Fractional t, MArray (STUArray s) t (ST s)) 
    => STUMatrix s t -> STUMatrix s t -> ST s ()
gaussj_stu = gaussj_generic

gaussj_generic a b = do
    n <- getNumRows a
    
    ipiv <- newDefaultRef S.empty
    indx <- newPermute n
    
    sequence_
        [ do
            -- pivot and return the column to work on.  icol will take
            -- each value 0..n-1 exactly once.
            icol <- pivot a b n ipiv indx
            
            piv <- readM a icol icol
            when (piv == 0) $ fail "gaussj: Singular Matrix"
            let pivinv = recip piv
            
            -- scale the pivot row of A by the amount that would
            -- make the diagonal element 1.  Because the diagonal element
            -- is now part of the inverse, though, it will be set to pivinv.
            writeM a icol icol 1
            mapRowM a icol (* pivinv)
            
            -- scale the corresponding row of the solution set
            mapRowM b icol (* pivinv)
            
            reduceRows a b n icol
        
        | i <- [0..n-1]
        ]
    
    unshuffleColumns indx a

-- select a cell to pivot on.
-- place that element on the diagonal by swapping rows, and log the swap for
-- unshuffleColumns to apply to the same columns of the inverse later
pivot a b n ipivRef indx = do
    ipiv <- readRef ipivRef
    (irow, icol) <- selectPivot a n ipiv
    writeRef ipivRef (S.insert icol ipiv)
    
    when (irow /= icol) $ do
        swapRows a irow icol
        swapRows b irow icol
        swapElems indx irow icol

    return icol

-- select the largest element in the unpivoted rows and columns of the matrix.
selectPivot a n ipiv = go is is 0 (error "no pivot selected")
    where
        is = filter (`S.notMember` ipiv) [0..n-1]
        
        go      []    _    big irc = return irc
        go    (j:js)  []   big irc = go js is big irc
        go js@(j:_) (k:ks) big irc = do
            x <- readM a j k
            let abs_x = abs x
            if abs_x >= big
                then go js ks abs_x (j,k)
                else go js ks big    irc

-- subtract linear combinations of the pivot row from every other row, using
-- the value that will cause all values in the column to become zero (or
-- would if it were not instead contributing to the value of the inverse
-- of A).  This simultaneously updates the remaining columns of A and the
-- already-produced columns of its inverse.  The same operation is also
-- done to B.
reduceRows a b n icol = sequence_
    [ do
        dum <- readM a ll icol
        writeM a ll icol 0
        
        zipRowsWithM a ll icol ll (\x y -> x - y * dum)
        zipRowsWithM b ll icol ll (\x y -> x - y * dum)
    | ll <- [0..n-1]
    , ll /= icol
    ]

-- put the columns of the inverse of A back in place, by inverting the
-- permutation applied by the 'pivot' function.
unshuffleColumns indx a = do
    swaps <- getInvSwaps indx
    sequence_
        [ swapCols a r c
        | (r, c) <- swaps
        , r /= c
        ]

-- helper thingy that needs to get 'simplified away'.  It's simple, but not
-- really idiomatic or even intuitive.
zipRowsWithM a r1 r2 r3 f = do
    n <- getNumCols a
    sequence_
        [ do
            x <- readM a r1 l
            y <- readM a r2 l
            writeM a r3 l (f x y)
        | l <- [0..n-1]
        ]
