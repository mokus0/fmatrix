{-
 - Purely functional (and lazy) forward- and back-substitution
 - (solving of lower and upper triangular systems, respectively)
 -}
module Data.Matrix.Algorithms.Substitution where

-- TODO: proper size checking
import Data.Matrix.Types
import Data.List

backSub :: (Fractional t, Matrix m t, Vector v t)
    => m t -> v t -> IVector t
backSub a b = x
    where
        n = matCols a
        x = vector n $ \i ->
            (indexV b i - sum [indexM a i j * indexV x j | j <- [i+1 .. n-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

forwardSub :: (Fractional t, Matrix m t, Vector v t)
    => m t -> v t -> IVector t
forwardSub a b = x
    where
        n = matCols a
        x = vector n $ \i ->
            (indexV b i - sum [indexM a i j * indexV x j | j <- [0 .. i-1]]) / indexM a i i
        
        sum [] = 0
        sum xs = foldl1' (+) xs

