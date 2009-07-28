{-# LANGUAGE 
        NoMonomorphismRestriction, 
        RecordWildCards,
        ExistentialQuantification
  #-}
module Data.Matrix.Functional where

import qualified Data.StorableVector as SV
import Foreign.Storable

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


-- contractWith :: (a -> b -> c) -> ([c] -> t) -> Matrix a -> Int -> Matrix b -> Matrix t
-- contractWith (*) sum m1 n m2 r c = sum
--     [ m1 r i * m2 i c
--     | i <- [0..n-1]
--     ]
-- 
-- mul :: Num a => Matrix a -> Int -> Matrix a -> Matrix a
-- mul m1 n m2 r c = sum
--     [ a * b
--     | i <- [0..n-1]
--     , let a = m1 r i
--     , a /= 0
--     , let b = m2 i c
--     , b /= 0
--     ]
-- 
-- apply :: Num t => Matrix t -> Vector t -> Int -> Vector t
-- apply mat vec r c = applyVec (row c mat) r vec
-- 
-- applyVec :: Num t => Vector t -> Int -> Vector t -> t
-- applyVec v1 n v2 = sum
--     [ v1 i * v2 i
--     | i <- [0..n-1]
--     ]
-- 
-- asCol :: Vector t -> Matrix t
-- asCol v r c = v r
-- 
-- col :: Int -> Matrix t -> Vector t
-- col c m r = m r c
-- 
-- asRow :: Vector t -> Matrix t
-- asRow v r c = v c
-- 
-- row :: Int -> Matrix t -> Vector t
-- row r m c = m r c
-- 
asDiagonal :: Num t => Vector t -> Matrix t
asDiagonal vec = matrix n n diag
    where
        n = vecElems vec
        diag r c
            | r == c    = vecIndex vec r
            | otherwise = 0

diagonal :: Matrix t -> Vector t
diagonal mat = vector (min r c) $ \n -> index mat n n
    where
        r = matRows mat
        c = matCols mat

kronecker :: Num t => Int -> Matrix t
kronecker n = asDiagonal (vector n (const 1))

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

-- 
-- vcat, hcat :: Int -> Matrix t -> Matrix t -> Matrix t
-- vcat r0 m1 m2 r c
--     | r < r0    = m1  r     c
--     | otherwise = m2 (r-r0) c
-- 
-- hcat c0 m1 m2 r c
--     | c < c0    = m1 r     c
--     | otherwise = m2 r (c-c0)
-- 
