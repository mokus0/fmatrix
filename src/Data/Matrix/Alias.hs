{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, EmptyDataDecls #-}
module Data.Matrix.Alias where

import Control.Monad.Identity
import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.Permute.MPermute
import Data.Monoid

data Mat
data Vec

type IAlias k = Alias k Identity
data Alias k m t where
    FMap        :: (a -> b) -> Alias k Identity a -> Alias k Identity b
    ZipWith     :: (a -> b -> c) -> Alias k Identity a -> Alias k Identity b -> Alias k Identity c
    MMat        :: MMatrix mat t m => mat t -> Alias Mat m t
    MVec        :: MVector vec t m => vec t -> Alias Vec m t
    Mat         :: Matrix mat t    => mat t -> Alias Mat m t
    Vec         :: Vector vec t    => vec t -> Alias Vec m t
    AsDiag      :: Alias Vec m t -> t -> Alias Mat m t
    AsRow       :: Alias Vec m t      -> Alias Mat m t
    AsCol       :: Alias Vec m t      -> Alias Mat m t
    Transpose   :: Alias Mat m t      -> Alias Mat m t
    Diag        :: Alias Mat m t      -> Alias Vec m t
    Row         :: Int -> Alias Mat m t -> Alias Vec m t
    Col         :: Int -> Alias Mat m t -> Alias Vec m t
    ShiftM      :: Int -> Int -> Alias Mat m t -> Alias Mat m t
    CropM       :: Int -> Int -> Alias Mat m t -> Alias Mat m t
    ShiftV      :: Int        -> Alias Vec m t -> Alias Vec m t
    CropV       :: Int        -> Alias Vec m t -> Alias Vec m t
    RowPermute  :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    ColPermute  :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    VecPermute  :: MPermute p m => p -> Alias Vec m t -> Alias Vec m t
    Overlay     :: Alias k m t -> Alias k m t -> Alias k m t
    Fill        :: t -> Alias k m t

type ICellAlias = CellAlias Identity
data CellAlias m t where
    FMapCell    :: (a -> b) -> CellAlias Identity a -> CellAlias Identity b
    ZipWithCell :: (a -> b -> c) -> CellAlias Identity a -> CellAlias Identity b -> CellAlias Identity c
    MMatCell    :: MMatrix mat t m => Int -> Int -> mat t -> CellAlias m t
    MVecCell    :: MVector vec t m => Int ->        vec t -> CellAlias m t
    ConstCell   :: t -> CellAlias m t
    ROConstCell :: t -> CellAlias m t
    NoCell      :: CellAlias m t

readICellM :: IAlias Mat t -> Int -> Int -> t
readICellM m i j = runIdentity (lookupAliasM m i j >>= readCell)

readICellV :: IAlias Vec t -> Int -> t
readICellV v i = runIdentity (lookupAliasV v i >>= readCell)

readCell :: Monad m => CellAlias m t -> m t
readCell (FMapCell f c)     = do
    x <- readCell c
    return (f x)
readCell (ZipWithCell f a b)     = do
    x <- readCell a
    y <- readCell b
    return (f x y)
readCell (MMatCell i j m)   = readM m i j
readCell (MVecCell i v)     = readV v i
readCell (ConstCell   t)    = return t
readCell (ROConstCell t)    = return t
readCell NoCell = fail "Alias.readCell: cell read was not aliased to anything"

writeCell :: Monad m => CellAlias m t -> t -> m ()
writeCell (MMatCell i j m)  x = writeM m i j x
writeCell (MVecCell i v)    x = writeV v i x
writeCell (ROConstCell t)   x = error "writeCell: cell written is read-only"
writeCell (ConstCell t)     x = return ()
writeCell NoCell x = fail "Alias.writeCell: cell written was not aliased to anything"

instance Monoid (CellAlias m t) where
    mempty = NoCell
    mappend NoCell x = x
    mappend x y      = x

inBndsM i j (r,c) = and
    [ i >= 0
    , j >= 0
    , i < r
    , j < c
    ]

lookupAliasM :: Monad m => Alias Mat m t -> Int -> Int -> m (CellAlias m t)
lookupAliasM (FMap f m) i j = do
    c <- lookupAliasM m i j
    return (FMapCell f c)
lookupAliasM (ZipWith f m1 m2) i j = do
    c1 <- lookupAliasM m1 i j
    c2 <- lookupAliasM m2 i j
    return (ZipWithCell f c1 c2)
lookupAliasM (Mat m) i j = do
    let sz = matSize m
    if inBndsM i j sz
        then return (ROConstCell (indexM m i j))
        else return NoCell
lookupAliasM (MMat m) i j = do
    sz <- getMatSize m
    if inBndsM i j sz
        then return (MMatCell i j m)
        else return NoCell
lookupAliasM (AsDiag v z) i j
    | i == j    = lookupAliasV v i
    | otherwise = return (ConstCell z)
lookupAliasM (AsRow v)        0 j = lookupAliasV v j
lookupAliasM (AsCol v)        i 0 = lookupAliasV v i
lookupAliasM (Transpose m)    i j = lookupAliasM m j i
lookupAliasM (ShiftM di dj m) i j = lookupAliasM m (i-di) (j-dj)
lookupAliasM (CropM r c m)    i j
    | inBndsM i j (r,c) = lookupAliasM m i j
    | otherwise = return NoCell
lookupAliasM (Overlay m1 m2)  i j = do
    c1 <- lookupAliasM m1 i j
    c2 <- lookupAliasM m2 i j
    return (c1 `mappend` c2)
lookupAliasM (RowPermute p m) i j = do
    i <- getElem p i
    lookupAliasM m i j
lookupAliasM (ColPermute p m) i j = do
    j <- getElem p j
    lookupAliasM m i j
lookupAliasM (Fill t) i j         = return (ConstCell t)
lookupAliasM _ _ _ = return NoCell

aliasSizeM :: Monad m => Alias Mat m t -> m (Int, Int)
aliasSizeM (FMap f m) = aliasSizeM m
aliasSizeM (ZipWith f m1 m2) = do
    (r1,c1) <- aliasSizeM m1
    (r2,c2) <- aliasSizeM m2
    return (min r1 r2, min c1 c2)
aliasSizeM (Mat m) = return (matSize m)
aliasSizeM (MMat m) = getMatSize m
aliasSizeM (AsDiag v z) = do
    i <- aliasSizeV v
    return (i,i)
aliasSizeM (AsRow v) = do
    i <- aliasSizeV v
    return (1,i)
aliasSizeM (AsCol v) = do
    i <- aliasSizeV v
    return (i,1)
aliasSizeM (RowPermute p m) = aliasSizeM m
aliasSizeM (ColPermute p m) = aliasSizeM m
aliasSizeM (ShiftM di dj m) = do
    (r,c) <- aliasSizeM m
    return (r+di, c+dj)
aliasSizeM (CropM r1 c1 m) = do
    (r2,c2) <- aliasSizeM m
    return (min r1 r2, min c1 c2)
aliasSizeM (Overlay m1 m2) = do
    (r1,c1) <- aliasSizeM m1
    (r2,c2) <- aliasSizeM m2
    return (max r1 r2, max c1 c2)
aliasSizeM (Fill t) = return (0,0)

inBndsV i n = (i >= 0) && (i < n)

lookupAliasV :: Monad m => Alias Vec m t -> Int -> m (CellAlias m t)
lookupAliasV (FMap f v) i = do
    c <- lookupAliasV v i
    return (FMapCell f c)
lookupAliasV (ZipWith f v1 v2) i = do
    c1 <- lookupAliasV v1 i
    c2 <- lookupAliasV v2 i
    return (ZipWithCell f c1 c2)
lookupAliasV (Vec v) i = do
    let n = vecElems v
    if inBndsV i n
        then return (ROConstCell (indexV v i))
        else return NoCell
lookupAliasV (MVec v) i = do
    n <- getVecSize v
    if inBndsV i n
        then return (MVecCell i v)
        else return NoCell
lookupAliasV (Row i m) j = lookupAliasM m i j
lookupAliasV (Col j m) i = lookupAliasM m i j
lookupAliasV (Diag m)  i = lookupAliasM m i i

lookupAliasV (ShiftV di v)   i = lookupAliasV v (i-di)
lookupAliasV (CropV  n v)    i 
    | inBndsV i n = lookupAliasV v i
    | otherwise = return NoCell
lookupAliasV (Overlay v1 v2) i = do
    c1 <- lookupAliasV v1 i
    c2 <- lookupAliasV v2 i
    return (c1 `mappend` c2)
lookupAliasV (VecPermute p v) i = do
    i <- getElem p i
    lookupAliasV v i
lookupAliasV (Fill t) i = return (ConstCell t)
lookupAliasV _ _ = return NoCell

aliasSizeV :: Monad m => Alias Vec m t -> m Int
aliasSizeV (FMap f v) = aliasSizeV v
aliasSizeV (ZipWith f v1 v2) = do
    n1 <- aliasSizeV v1
    n2 <- aliasSizeV v2
    return (min n1 n2)
aliasSizeV (MVec m) = getVecSize m
aliasSizeV (VecPermute p m) = aliasSizeV m
aliasSizeV _ = error "aliasSizeV: not completely implemented"




aliasMatrixWith f m = f (MMat m)

instance Linear (Alias Mat Identity) t where
    liftLinear = FMap
    liftLinear2 = ZipWith
    
instance Matrix (Alias Mat Identity) t where
    matSize = runIdentity . aliasSizeM
    matrix r c m = (Mat :: FunctionMatrix a -> IAlias Mat a) (matrix r c m)
    unsafeIndexM = readICellM
    
instance Monad m => MMatrix (Alias Mat m) t m where
    newMatrix r c m = return ((Mat :: FunctionMatrix a -> Alias Mat m a) (matrix r c m))
    readM m i j = do
        cell <- lookupAliasM m i j
        readCell cell
        
    writeM m i j x = do
        cell <- lookupAliasM m i j
        writeCell cell x
        
    getMatSize = aliasSizeM

aliasVectorWith f v = f (MVec v)

aliasRow, aliasCol :: MMatrix mat t m => mat t -> Int -> m (Alias Vec m t)
aliasRow m i = return (Row i (MMat m))
aliasCol m j = return (Row j (MMat m))


instance Functor (Alias k Identity) where
    fmap = FMap
instance Linear (Alias Vec Identity) t where
    liftLinear = fmap
    liftLinear2 = ZipWith
instance Vector (Alias Vec Identity) t where
    vecElems = runIdentity . aliasSizeV
    vector n v = (Vec :: FunctionVector a -> IAlias Vec a) (vector n v)
    unsafeIndexV = readICellV

instance Monad m => MVector (Alias Vec m) t m where
    newVector n v = return ((Vec :: FunctionVector a -> Alias Vec m a) (vector n v))
    readV v i = do
        cell <- lookupAliasV v i
        readCell cell
        
    writeV v i x = do
        cell <- lookupAliasV v i
        writeCell cell x
        
    getVecSize = aliasSizeV

