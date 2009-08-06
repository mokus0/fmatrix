{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, EmptyDataDecls #-}
module Data.Matrix.Alias where

import Data.Matrix.Mutable
import Data.Permute.MPermute
import Data.Monoid

data Mat
data Vec

data Alias k m t where
    Mat         :: MMatrix mat t m => mat t -> Alias Mat m t
    Vec         :: MVector vec t m => vec t -> Alias Vec m t
    AsDiag      :: Alias Vec m t -> Alias Mat m t
    AsRow       :: Alias Vec m t -> Alias Mat m t
    AsCol       :: Alias Vec m t -> Alias Mat m t
    Transpose   :: Alias Mat m t -> Alias Mat m t
    Diag        :: Alias Mat m t -> Alias Vec m t
    Row         :: Int -> Alias Mat m t -> Alias Vec m t
    Col         :: Int -> Alias Mat m t -> Alias Vec m t
    ShiftM      :: Int -> Int -> Alias Mat m t -> Alias Mat m t
    ShiftV      :: Int -> Alias Vec m t -> Alias Vec m t
    Overlay     :: Alias k m t -> Alias k m t -> Alias k m t
    RowPermute  :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    ColPermute  :: MPermute p m => p -> Alias Mat m t -> Alias Mat m t
    VecPermute  :: MPermute p m => p -> Alias Vec m t -> Alias Vec m t

data CellAlias m t where
    MatCell :: MMatrix mat t m => Int -> Int -> mat t -> CellAlias m t
    VecCell :: MVector vec t m => Int ->        vec t -> CellAlias m t
    NoCell  :: CellAlias m t

readCell :: Monad m => CellAlias m t -> m t
readCell (MatCell i j m) = readM m i j
readCell (VecCell i v)   = readV v i
readCell NoCell = error "readCell: cell read was not mapped to anything"

writeCell :: Monad m => CellAlias m t -> t -> m ()
writeCell (MatCell i j m) x = writeM m i j x
writeCell (VecCell i v)   x = writeV v i x
writeCell NoCell x = error "readCell: cell read was not mapped to anything"

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

lookupAliasM :: Monad m => Int -> Int -> Alias Mat m t -> m (CellAlias m t)
lookupAliasM i j (Mat m) = do
    sz <- getMatSize m
    if inBndsM i j sz
        then return (MatCell i j m)
        else return NoCell
lookupAliasM i j (AsDiag v)
    | i == j    = lookupAliasV i v
lookupAliasM 0 j (AsRow v) = lookupAliasV j v
lookupAliasM i 0 (AsCol v) = lookupAliasV i v
lookupAliasM i j (Transpose m) = lookupAliasM j i m
lookupAliasM i j (ShiftM di dj m) = lookupAliasM (i-di) (j-dj) m
lookupAliasM i j (Overlay m1 m2) = do
    c1 <- lookupAliasM i j m1
    c2 <- lookupAliasM i j m2
    return (c1 `mappend` c2)
lookupAliasM i j (RowPermute p m) = do
    i <- getElem p i
    lookupAliasM i j m
lookupAliasM i j (ColPermute p m) = do
    j <- getElem p j
    lookupAliasM i j m

lookupAliasM _ _ _ = return NoCell

aliasSizeM :: Monad m => Alias Mat m t -> m (Int, Int)
aliasSizeM (Mat m) = getMatSize m
aliasSizeM (AsDiag v) = do
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
aliasSizeM _ = error "aliasSizeM: not completely implemented"

inBndsV i n = (i >= 0) && (i < n)

lookupAliasV :: Monad m => Int -> Alias Vec m t -> m (CellAlias m t)
lookupAliasV i (Vec v) = do
    n <- getVecSize v
    if inBndsV i n
        then return (VecCell i v)
        else return NoCell
lookupAliasV i (Row j m) = lookupAliasM i j m
lookupAliasV j (Col i m) = lookupAliasM i j m
lookupAliasV i (Diag m)  = lookupAliasM i i m

lookupAliasV i (ShiftV di v) = lookupAliasV (i-di) v
lookupAliasV i (Overlay v1 v2) = do
    c1 <- lookupAliasV i v1
    c2 <- lookupAliasV i v2
    return (c1 `mappend` c2)

lookupAliasV i (VecPermute p v) = do
    i <- getElem p i
    lookupAliasV i v

lookupAliasV _ _ = return NoCell

aliasSizeV :: Monad m => Alias Vec m t -> m Int
aliasSizeV (Vec m) = getVecSize m
aliasSizeV (VecPermute p m) = aliasSizeV m
aliasSizeV _ = error "aliasSizeV: not completely implemented"




aliasMatrixWith f m = f (Mat m)

instance Monad m => MMatrix (Alias Mat m) t m where
    newMatrix r c m = error "Alias matrices cannot be created by newMatrix"
    readM m i j = do
        cell <- lookupAliasM i j m
        readCell cell
        
    writeM m i j x = do
        cell <- lookupAliasM i j m
        writeCell cell x
        
    getMatSize = aliasSizeM

aliasVectorWith f v = f (Vec v)

instance Monad m => MVector (Alias Vec m) t m where
    newVector n v = error "Alias vectors cannot be created by newVector"
    readV v i = do
        cell <- lookupAliasV i v
        readCell cell
        
    writeV v i x = do
        cell <- lookupAliasV i v
        writeCell cell x
        
    getVecSize = aliasSizeV

