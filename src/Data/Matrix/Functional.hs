{-# LANGUAGE 
        NoMonomorphismRestriction, 
        RecordWildCards,
        ExistentialQuantification
  #-}
module Data.Matrix.Functional where

import qualified Data.StorableVector as SV
import Foreign.Storable

import Prelude hiding (sum)

import SimpleReflect
mat n r c = fromList [[fun n i j :: Expr | j <- [0..c-1]] | i <- [0..r-1]]

sum xs
    | null xs   = 0
    | otherwise = foldl1 (+) xs

data Vector t
    = Storable t => Vector
        { vecBuf    :: SV.Vector t
        , vecPos    :: !Int
        , vecElems  :: !Int
        , vecStride :: !Int
        }
    | VectorFunc
        { vecElems  :: !Int
        , vecFunc   :: Int -> t
        }

data Matrix t 
    = Storable t => Matrix
        { matBuf    :: SV.Vector t
        , matPos    :: !Int
        , matRows   :: !Int
        , matRowD   :: !Int
        , matCols   :: !Int
        , matColD   :: !Int
        }
    | MatrixFunc
        { matRows   :: !Int
        , matCols   :: !Int
        , matFunc   :: Int -> Int -> t
        }

instance Show t => Show (Vector t) where
    showsPrec p vec = showString (unlines . map show . vecToList $ vec)

instance Show t => Show (Matrix t) where
    showsPrec p mat = showString (unlines . map show . toList $ mat)

tabulate :: Storable t => Matrix t -> Matrix t
tabulate mat@Matrix{} = mat
tabulate mat@MatrixFunc{..} = packRowMajor matRows matCols (index mat)

packRowMajor :: Storable t => Int -> Int -> (Int -> Int -> t) -> Matrix t
packRowMajor r c m = Matrix
    { matRows   = r
    , matRowD   = c
    , matCols   = c
    , matColD   = 1
    , matPos    = 0
    , matBuf    = SV.pack [m i j | i <- [0..r-1], j <- [0..c-1]]
    }

packColMajor :: Storable t => Int -> Int -> (Int -> Int -> t) -> Matrix t
packColMajor r c m = Matrix
    { matRows   = r
    , matRowD   = 1
    , matCols   = c
    , matColD   = r
    , matPos    = 0
    , matBuf    = SV.pack [m i j | j <- [0..c-1], i <- [0..r-1]]
    }

index :: Matrix t -> Int -> Int -> t
index mat r c
    | r < 0             = error "foo"
    | r >= matRows mat  = error "foo"
    | c < 0             = error "foo"
    | c >= matCols mat  = error "foo"
    | otherwise         = unsafeIndex mat r c

unsafeIndex mat@Matrix{..}     r c = SV.index matBuf (bufIndex mat r c)
unsafeIndex mat@MatrixFunc{..} r c = matFunc r c

bufIndex Matrix{..} r c = matPos + r * matRowD + c * matColD

matrix :: Int -> Int -> (Int -> Int -> t) -> Matrix t
matrix = MatrixFunc

fromList :: [[t]] -> Matrix t
fromList m = matrix (length m) (minimum (map length m)) $ \r c -> m !! r !! c

permuteRows    f m = matrix (matRows m) (matCols m) $ \i j -> index m (f i) j
permuteCols    f m = matrix (matRows m) (matCols m) $ \i j -> index m i (f j)
permuteIndices f m = matrix (matRows m) (matCols m) $ \i j -> uncurry (index m) (f i j)

toList m =
    [ [ index m i j
      | j <- [0..matCols m - 1]
      ]
    | i <- [0..matRows m - 1]
    ]

vecIndex v i
    | i < 0             = error "bar"
    | i >= vecElems v   = error "bar"
    | otherwise         = unsafeVecIndex v i

unsafeVecIndex vec@Vector{..}     i = SV.index vecBuf (vecBufIndex vec i)
unsafeVecIndex vec@VectorFunc{..} i = vecFunc i

vecBufIndex v@Vector{..} i = vecPos + vecStride * i

vecTabulate vec@Vector{}    = vec
vecTabulate vec             = Vector
    { vecBuf    = SV.pack (vecToList vec)
    , vecPos    = 0
    , vecElems  = vecElems vec
    , vecStride = 1
    }

vector :: Int -> (Int -> t) -> Vector t
vector = VectorFunc

vecFromList l = vector (length l) (l!!)

vecToList v = [vecIndex v i | i <- [0..vecElems v - 1]]

asDiagonal :: t -> Vector t -> Matrix t
asDiagonal z vec = matrix n n diag
    where
        n = vecElems vec
        diag r c
            | r == c    = vecIndex vec r
            | otherwise = z

diagonal :: Matrix t -> Vector t
diagonal mat = vector (min r c) $ \n -> index mat n n
    where
        r = matRows mat
        c = matCols mat

kronecker :: Num t => Int -> Matrix t
kronecker n = asDiagonal 0 (vector n (const 1))

transpose :: Matrix t -> Matrix t
transpose mat@Matrix{} = Matrix
    { matBuf  = matBuf  mat
    , matPos  = matPos  mat
    , matRows = matCols mat
    , matRowD = matColD mat
    , matCols = matRows mat
    , matColD = matRowD mat
    }
transpose (MatrixFunc r c f) = MatrixFunc c r (flip f)

apply :: Num t => Matrix t -> Vector t -> Vector t
apply = applyWith sum (*)

applyWith :: ([c] -> t) -> (a -> b -> c) -> Matrix a -> Vector b -> Vector t
applyWith sum (*) m v 
    | matCols m == vecElems v
    = vector (matRows m) $ \i ->
        sum [index m i j * vecIndex v j | j <- [0..matCols m - 1]]
    | otherwise
    = error "apply: matrix does not have same number of columns as vector has elements"

multiply :: Num t => Matrix t -> Matrix t -> Matrix t
multiply = multiplyWith sum (*)

multiplyWith sum (*) m1 m2
    | matCols m1 == matRows m2
    = matrix (matRows m1) (matCols m2) $ \i j ->
        sum [index m1 i k * index m2 k j | k <- [0..matCols m1-1]]
    
    | otherwise
    = error "multiplyWith: matrices' sizes are not compatible"

