{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, FlexibleInstances #-}
module Data.Matrix.Types where

import qualified Data.StorableVector as SV
import Foreign.Storable

class Matrix m t where
    matRows         :: m t -> Int
    matCols         :: m t -> Int
    matrix          :: Int -> Int -> (Int -> Int -> t) -> m t
    unsafeIndexM    :: m t -> (Int -> Int -> t)

class Vector v t where
    vecElems        :: v t -> Int
    vector          :: Int -> (Int -> t) -> v t
    unsafeIndexV    :: v t -> (Int -> t)

data StorableMatrix t 
    = StorableMatrix
        { smBuf    :: SV.Vector t
        , smPos    :: !Int
        , smRows   :: !Int
        , smRowD   :: !Int
        , smCols   :: !Int
        , smColD   :: !Int
        }

instance (Show t, Storable t) => Show (StorableMatrix t) where
    showsPrec p = showsMatrix

instance Storable t => Matrix StorableMatrix t where
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

instance Matrix FunctionMatrix t where
    matRows = fmRows
    matCols = fmCols
    matrix = FunctionMatrix
    unsafeIndexM = fmFunc

showsMatrix mat = showString (unlines . map show . matrixToList $ mat)

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

matrixFromList :: Matrix m t => [[t]] -> m t
matrixFromList m = matrix (length m) (minimum (map length m)) $ \r c -> m !! r !! c

convertM :: (Matrix m1 a, Matrix m2 a) => m1 a -> m2 a
convertM = convertByM id
convertByM :: (Matrix m1 a, Matrix m2 b) => (a -> b) -> m1 a -> m2 b
convertByM f m = matrix (matRows m) (matCols m) (\i j -> f (unsafeIndexM m i j))

convertV :: (Vector v1 a, Vector v2 a) => v1 a -> v2 a
convertV = convertByV id
convertByV :: (Vector v1 a, Vector v2 b) => (a -> b) -> v1 a -> v2 b
convertByV f v = vector (vecElems v) (\i -> f (unsafeIndexV v i))
