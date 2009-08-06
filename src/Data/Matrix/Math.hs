module Data.Matrix.Math where

import Data.Matrix.Types
import Data.List

{-# INLINE apply #-}
apply :: (Num t, Matrix m t, Vector v t) => m t -> v t -> v t
apply = genericApply

{-# INLINE genericApply #-}
genericApply :: (Num t, Matrix m t, Vector v1 t, Vector v2 t) => m t -> v1 t -> v2 t
genericApply = applyWith (+) (*)

{-# INLINE applyWith #-}
applyWith :: (Matrix m a, Vector v1 b, Vector v2 c) => (c -> c -> c) -> (a -> b -> c) -> m a -> v1 b -> v2 c
applyWith (+) (*) m v 
    | matCols m == vecElems v
    = vector (matRows m) $ \i ->
        foldl1' (+) [unsafeIndexM m i j * unsafeIndexV v j | j <- [0..matCols m - 1]]
    
    | otherwise
    = error "apply: matrix does not have same number of columns as vector has elements"


{-# INLINE multiply #-}
multiply :: (Num t, Matrix m t) => m t -> m t -> m t
multiply = genericMultiply

{-# INLINE genericMultiply #-}
genericMultiply :: (Num t, Matrix m1 t, Matrix m2 t, Matrix m3 t) => m1 t -> m2 t -> m3 t
genericMultiply = multiplyWith (foldl1' (+)) (*)

{-# INLINE multiplyWith #-}
multiplyWith :: (Matrix m1 a, Matrix m2 b, Matrix m3 c)
                => ([t] -> c)
                -> (a -> b -> t)
                -> m1 a -> m2 b -> m3 c
multiplyWith sum (*) m1 m2
    | matCols m1 == matRows m2
    = matrix (matRows m1) (matCols m2) $ \i j ->
        sum [unsafeIndexM m1 i k * unsafeIndexM m2 k j | k <- [0..matCols m1-1]]
    
    | otherwise
    = error "multiplyWith: matrices' sizes are not compatible"

