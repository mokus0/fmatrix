{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances,FlexibleContexts, UndecidableInstances #-}
module Data.Matrix.Types where

import qualified Data.StorableVector as SV
import Foreign.Storable

import Data.Array.IArray
import Data.Array.Unboxed

import Data.Permute

class Linear m a where
    liftLinear  :: (a -> a) -> m a -> m a
    liftLinear2 :: (a -> a -> a) -> m a -> m a -> m a

scale k = liftLinear (k*)
addL a b = liftLinear2 (+) a b
subL a b = liftLinear2 (-) a b
mulL a b = liftLinear2 (*) a b
divL a b = liftLinear2 (/) a b

class Linear m t => Matrix m t where
    matSize         :: m t -> (Int, Int)
    matRows         :: m t -> Int
    matRows = fst . matSize
    matCols         :: m t -> Int
    matCols = snd . matSize
    matrix          :: Int -> Int -> (Int -> Int -> t) -> m t
    unsafeIndexM    :: m t -> (Int -> Int -> t)

class Linear v t => Vector v t where
    vecElems        :: v t -> Int
    vector          :: Int -> (Int -> t) -> v t
    unsafeIndexV    :: v t -> (Int -> t)

newtype ArrayMatrix a t = ArrayMatrix { unArrayMatrix :: (a (Int, Int) t) }

instance Functor (a (Int, Int)) => Functor (ArrayMatrix a) where
    fmap f (ArrayMatrix m) = ArrayMatrix (fmap f m)

instance (IArray a t) => Linear (ArrayMatrix a) t
    where
        liftLinear f (ArrayMatrix a) = ArrayMatrix (amap f a)
        liftLinear2 = liftMatrix2
instance (IArray a t) => Matrix (ArrayMatrix a) t
    where
        matSize (ArrayMatrix m) = case bounds m of
            ((0,0), (r,c)) -> (r+1, c+1)
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

instance Linear [] t
    where
        liftLinear = fmap
        liftLinear2 = zipWith
instance Vector [] t
    where
        vector n v = [v i | i <- [0..n-1]]
        vecElems = length
        unsafeIndexV = (!!)

newtype ArrayVector a t = ArrayVector { unArrayVector :: (a Int t) }

instance (IArray a t) => Linear (ArrayVector a) t
    where
        liftLinear f (ArrayVector a) = ArrayVector (amap f a)
        liftLinear2 = liftVector2
instance (IArray a t) => Vector (ArrayVector a) t
    where
        vecElems (ArrayVector m) = case bounds m of
            (0,n) -> n+1
        vector n v = ArrayVector $ listArray (0,n-1)
            [ v i
            | i <- [0..n-1]
            ]
        unsafeIndexV (ArrayVector v) i = v ! i

instance (IArray a t, Show t) => Show (ArrayVector a t) where
    showsPrec p = showsVector

type IVector = ArrayVector Array
type UVector = ArrayVector UArray

data FunctionVector t
    = FunctionVector
        { fvSize    :: !Int
        , fvFunc    :: Int -> t
        }

instance Show t => Show (FunctionVector t)
    where
        showsPrec p = showsVector

instance Functor FunctionVector
    where
        fmap f (FunctionVector n v) = FunctionVector n (f.v)
instance Linear FunctionVector t
    where
        liftLinear = fmap
        liftLinear2 = liftVector2
instance Vector FunctionVector t
    where
        vecElems = fvSize
        vector = FunctionVector
        unsafeIndexV = fvFunc

data StorableMatrix t 
    = StorableMatrix
        { smBuf    :: SV.Vector t
        , smPos    :: !Int
        , smRows   :: !Int
        , smRowD   :: !Int
        , smCols   :: !Int
        , smColD   :: !Int
        }

instance (Show t, Storable t) => Show (StorableMatrix t)
    where
        showsPrec p = showsMatrix

instance Storable t => Linear StorableMatrix t
    where
        liftLinear = liftMatrix
        liftLinear2 = liftMatrix2

instance Storable t => Matrix StorableMatrix t
    where
        matSize m       = (smRows m, smCols m)
        matRows         = smRows
        matCols         = smCols
        matrix          = packRowMajor
        unsafeIndexM    = unsafeIndexSM

packRowMajor :: Storable t => Int -> Int -> (Int -> Int -> t) -> StorableMatrix t
packRowMajor r c m = StorableMatrix
    { smRows   = r
    , smRowD   = c
    , smCols   = c
    , smColD   = 1
    , smPos    = 0
    , smBuf    = SV.pack [m i j | i <- [0..r-1], j <- [0..c-1]]
    }

packColMajor :: Storable t => Int -> Int -> (Int -> Int -> t) -> StorableMatrix t
packColMajor r c m = StorableMatrix
    { smRows   = r
    , smRowD   = 1
    , smCols   = c
    , smColD   = r
    , smPos    = 0
    , smBuf    = SV.pack [m i j | j <- [0..c-1], i <- [0..r-1]]
    }

unsafeIndexSM StorableMatrix{..} r c = SV.index smBuf (smPos + r * smRowD + c * smColD)

data FunctionMatrix t
    = FunctionMatrix
        { fmRows   :: !Int
        , fmCols   :: !Int
        , fmFunc   :: Int -> Int -> t
        }

instance Show t => Show (FunctionMatrix t) where
    showsPrec p = showsMatrix

instance Functor FunctionMatrix where
    fmap f (FunctionMatrix r c m) = FunctionMatrix r c (\i j -> f (m i j))
instance Linear FunctionMatrix t where
    liftLinear = fmap
    liftLinear2 f (FunctionMatrix r1 c1 m1) (FunctionMatrix r2 c2 m2) =
        FunctionMatrix (min r1 r2) (min r2 c2) (\i j -> f (m1 i j) (m2 i j))
instance Matrix FunctionMatrix t where
    matSize m = (fmRows m, fmCols m)
    matRows = fmRows
    matCols = fmCols
    matrix = FunctionMatrix
    unsafeIndexM = fmFunc

showMatrix mat = showsMatrix mat ""
showsMatrix mat = showString (unlines . map show . matrixToList $ mat)
showVector vec = showsVector vec ""
showsVector vec = showString (unlines . map show . vectorToList $ vec)

indexM :: Matrix m t => m t -> Int -> Int -> t
indexM mat r c
    | r < 0             = error "foo"
    | r >= matRows mat  = error "foo"
    | c < 0             = error "foo"
    | c >= matCols mat  = error "foo"
    | otherwise         = unsafeIndexM mat r c

indexV :: Vector v t => v t -> Int -> t
indexV vec i
    | i < 0             = error "foo"
    | i >= vecElems vec = error "foo"
    | otherwise         = unsafeIndexV vec i


matrixToList :: Matrix m t => m t -> [[t]]
matrixToList m =
    [ [ indexM m i j
      | j <- [0..matCols m - 1]
      ]
    | i <- [0..matRows m - 1]
    ]

vectorToList :: Vector v t => v t -> [t]
vectorToList v =
    [ indexV v i
    | i <- [0..vecElems v - 1]
    ]

triDiag a b c = matrix n n f
    where
        n = minimum [length a, length b, length c]
        f i j = case j - i of
            -1  -> indexV a i
            0   -> indexV b i
            1   -> indexV c i
            _   -> 0

matrixFromList :: Matrix m t => [[t]] -> m t
matrixFromList m = matrix (length m) (minimum (map length m)) $ \r c -> m !! r !! c

vectorFromList :: Vector v t => [t] -> v t
vectorFromList v = vector (length v) (v!!)

imatrix :: Int -> Int -> (Int -> Int -> t) -> IMatrix t
imatrix = matrix
ivector :: Int -> (Int -> t) -> IVector t
ivector = vector

fmatrix :: Int -> Int -> (Int -> Int -> t) -> FunctionMatrix t
fmatrix = matrix
fvector :: Int -> (Int -> t) -> FunctionVector t
fvector = vector

convertM :: (Matrix m1 a, Matrix m2 a) => m1 a -> m2 a
convertM = convertByM id
convertByM :: (Matrix m1 a, Matrix m2 b) => (a -> b) -> m1 a -> m2 b
convertByM f m = matrix (matRows m) (matCols m) (\i j -> f (unsafeIndexM m i j))

convertV :: (Vector v1 a, Vector v2 a) => v1 a -> v2 a
convertV = convertByV id
convertByV :: (Vector v1 a, Vector v2 b) => (a -> b) -> v1 a -> v2 b
convertByV f v = vector (vecElems v) (\i -> f (unsafeIndexV v i))

permuteRows :: Matrix m a => Permute -> m a -> FunctionMatrix a
permuteRows indx m = matrix (matRows m) (matCols m) (\i j -> indexM m (indx `at` i) j)

permuteV :: Vector v a => Permute -> v a -> FunctionVector a
permuteV indx v = vector (vecElems v) (indexV v . at indx)

kronecker n = matrix n n $ \i j -> if i==j then 1 else 0

liftMatrix :: (Matrix m a, Matrix m b) => (a -> b) -> m a -> m b
liftMatrix = convertByM
liftVector :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
liftVector = convertByV

liftMatrix2 :: (Matrix m a, Matrix m b, Matrix m c)
    => (a -> b -> c) -> m a -> m b -> m c
liftMatrix2 = convertByM2

liftVector2 :: (Vector v a, Vector v b, Vector v c)
    => (a -> b -> c) -> v a -> v b -> v c
liftVector2 = convertByV2

convertByM2 (+) m1 m2 = matrix r c $ \i j -> indexM m1 i j + indexM m2 i j
    where
        r1 = matRows m1
        r | r1 == matRows m2    = r1
          | otherwise = error "liftMatrix2: matrix sizes don't match"
        c1 = matCols m1
        c | c1 == matCols m2    = c1
          | otherwise = error "liftMatrix2: matrix sizes don't match"

convertByV2 (+) v1 v2 = vector n $ \i -> indexV v1 i + indexV v2 i
    where
        n1 = vecElems v1
        n | n1 == vecElems v2 = n1
          | otherwise = error "liftVector2: vector sizes don't match"
