module Data.Matrix.Algorithms.Banded where

import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.Permute
import Data.Permute.MPermute
import Data.StateRef
import Control.Monad
import Control.Monad.ST

-- test
import Data.Matrix.Alias
import Data.Matrix.Math
a :: IMatrix Double
a  = matrixFromList [[1,3,-2,1],[3,5,6,2],[2,4,3,3],[1,2,7,4],[2,3,5,5]]
m1 = 1
m2 = 2
b :: IVector Double
b  = vectorFromList [5,7,8,2,19] 
a' :: IAlias Mat Double
a' = AsBandDiag m1 (IMat a) `Overlay` Fill 0
dcmp = bandec m1 m2 a
x :: IVector Double
x  = bandec_solve dcmp b
b' :: IVector Double
b' = a' `apply` x
b'' :: IVector Double
b'' = banmul m1 m2 a x


banmul m1 m2 a x = vector n $ \i -> sum
    [ indexM a i j * indexV x (j+k)
    | let k = i-m1
    , j <- [max 0 (negate k) .. min (m1+m2) (n-k-1)]
    ]
    
    where n = matRows a

data Bandec t = Bandec
    { bandec_n      :: Int
    , bandec_m1     :: Int
    , bandec_m2     :: Int
    , bandec_u      :: IMatrix t
    , bandec_l      :: IMatrix t
--    , bandec_indx   :: Permute
    , bandec_indx   :: IVector Int
    , bandec_d      :: Bool
    }

bandec m1 m2 a = runST (bandec_st m1 m2 a)
bandec_st m1 m2 a = do
    let n = matRows a
        mm = m1+m2+1
    --indx <- newPermute n
    indx <- newVector_ n :: ST s (STVector s Int)
    l <- newDefaultRef m1
    
    au <- copyMatrix a      :: ST s (STMatrix s Double) -- for now, generalize later
    al <- newMatrix_ n m1   :: ST s (STMatrix s Double) -- for now, generalize later
    
    -- rearrange storage
    sequence_
        [ do
            l_ <- readRef l
            sequence_
                [ do
                    x <- readM au i j
                    writeM au i (j-l_) x
                | j <- [m1-i .. mm-1]
                ]
            writeRef l (pred l_)
            l_ <- readRef l
            sequence_
                [ do
                    writeM au i j 0
                | j <- [mm-l_-1 .. mm-1]
                ]
        | i <- [0..m1-1]
        ]
    
    d <- newDefaultRef True
    writeRef l m1
    
    sequence_
        [ do
            dum <- readM au k 0
            dum <- newDefaultRef dum
            i <- newDefaultRef k
            
            l_ <- readRef l
            when (l_<n) $ modifyRef l succ
            l_ <- readRef l
            
            -- find the pivot element
            sequence_
                [ do
                    dum_ <- readRef dum
                    au_j_0 <- readM au j 0
                    when (abs au_j_0 > abs dum_) $ do
                        writeRef dum au_j_0
                        writeRef i j
                | j <- [k+1 .. l_-1]
                ]
            
            i <- readRef i
            dum <- readRef dum
            
            when (dum == 0) (fail "bandec: algorithmically singular matrix")
            writeV indx k i
            when (i /= k) $ do
                modifyRef d not
--                swapElems indx i k
                swapRowsM au   i k
            
            sequence_
                [ do
                    au_i_0 <- readM au i 0
                    au_k_0 <- readM au k 0
                    let dum = au_i_0 / au_k_0
                    writeM al k (i-k-1) dum
                    
                    sequence_
                        [ do
                            au_i_j <- readM au i j
                            au_k_j <- readM au k j
                            writeM au i (j-1) (au_i_j - dum * au_k_j)
                        | j <- [1..mm-1]
                        ]
                    writeM au i (mm-1) 0
                | i <- [k+1 .. l_-1]
                ]
        | k <- [0..n-1]
        ]
    
    au <- unsafeFreezeMatrix au
    al <- unsafeFreezeMatrix al
--    indx <- unsafeFreeze indx
    indx <- unsafeFreezeVector indx
    d <- readRef d
    return (Bandec n m1 m2 au al indx d)

bandec_solve (Bandec n m1 m2 au al indx d) b = runSTVector $ do
    x <- copyVector b :: ST s (STVector s Double)
--    x <- copyVector (permuteV indx b) :: ST s (STVector s Double)
    let mm = m1+m2+1
    l <- newDefaultRef m1
    sequence_
        [ do
--            let j = inverse indx `at` k
            let j = indexV indx k
            when (j /= k) $ swapVecElems x k j
            
            l_ <- readRef l
            when (l_ < n) $ writeRef l (succ l_)
            l_ <- readRef l
            sequence_
                [ do
                    let al_k_foo = indexM al k (j-k-1)
                    x_k <- readV x k
                    modifyV x j (subtract (al_k_foo * x_k))
                | j <- [k+1 .. l_-1]
                ]
        | k <- [0 .. n-1]
        ]
    writeRef l 1
    sequence_
        [ do
            dum <- readV x i
            dum <- newDefaultRef dum
            
            l_ <- readRef l
            sequence_
                [ do
                    let au_i_k = indexM au i k
                    x_kpi <- readV x (k+i)
                    modifyRef dum (subtract (au_i_k * x_kpi))
                | k <- [1 .. l_-1]
                ]
            dum <- readRef dum
            writeV x i (dum / indexM au i 0)
            when (l_<mm) $ modifyRef l succ
        | i <- [n-1,n-2 .. 0]
        ]
    return x