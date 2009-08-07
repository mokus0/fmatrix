module Data.Matrix.Algorithms.Tridag where

import Data.Matrix.Math
import Data.Matrix.Types
import Data.Matrix.Mutable
import Data.StateRef
import Control.Monad
import Data.List
import Data.Array.MArray (MArray)

-- test case
a = [2,3,5,7,11]
b = [13,17,19,23,29]
c = [31,37,41,49,53]
m = fromLists a b c :: IMatrix Rational
r = [59,67,71,73,79]
av, bv, cv, rv :: IVector Rational
av = vectorFromList a
bv = vectorFromList b
cv = vectorFromList c
rv = vectorFromList r
x :: IVector Rational
x = runSTVector (tridag av bv cv rv)
-- chk should be all zeroes
chk = m `apply` x `subL` rv



fromLists a b c = matrix n n f
    where
        n = minimum [length a, length b, length c]
        f i j = case j - i of
            -1  -> a !! i
            0   -> b !! i
            1   -> c !! i
            _   -> 0

tridag a b c r = do
    let (gam, u) = decomp_and_fsub a b c r
    u   <- unsafeThawVector u
    backsub gam u
    return u

decomp_and_fsub a b c r = (ivec gam, ivec u)
    where
        ivec :: [t] -> IVector t
        ivec xs = vector n (xs!!)
        n = vecElems r
        
        (gam,u) = unzip gam_u
        gam_u = (gam0, u0) : snd (mapAccumL next_gam_u (b0, u0) [1..n-1])
        next_gam_u (prevBet, prevU) j = ((bet_j, u_j), (gam_j, u_j))
            where
                bet_j = indexV b j - a_j  * gam_j
                gam_j = indexV c (j-1) / prevBet
                u_j   = (indexV r j - a_j * prevU) / bet_j
                
                a_j = indexV a j
        
        b0 = indexV b 0
        u0   = indexV r 0 / b0
        gam0 = 0

backsub gam u = do
    n <- getVecSize u
    sequence_
        [ do
            let gam_j = indexV gam j
            u_j   <- readV u   j
            modifyV u (j-1) (subtract (gam_j * u_j))
        | j <- [n-1, n-2 .. 1]
        ]
