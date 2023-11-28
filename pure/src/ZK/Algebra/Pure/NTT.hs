
-- | Number Theoretic Transform

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module ZK.Algebra.Pure.NTT
  ( polyEvaluate
  , polyInterpolate
  , ntt
  , intt
  ) 
  where

--------------------------------------------------------------------------------

import Data.Array

import ZK.Algebra.Pure.Misc
import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Poly

--------------------------------------------------------------------------------

-- | First n powers of g
powers :: Field f => Int -> f -> [f]
powers n g = go 1 n where
  go  _  0 = []
  go !y !n = y : go (g*y) (n-1)

--------------------------------------------------------------------------------

-- | Evaluate the polynomial on a multiplicative subgroup, using FFT 
polyEvaluate :: Field f => MulSubgroup f -> Poly f -> Array Int f 
polyEvaluate = ntt

-- | Interpolate the values on a multiplicative subgroup into a polynomial, using FFT 
polyInterpolate :: Field f => MulSubgroup f -> Array Int f -> Poly f 
polyInterpolate = intt

--------------------------------------------------------------------------------

-- | Evaluates a polynomial on a subgroup
ntt :: Field f => MulSubgroup f -> Poly f -> Array Int f 
ntt subgroup (Poly coeffs)
  | n1+1 /= mulSubgroupOrder subgroup = error "ntt: input size does not match the subgroup order"
  | n1 == 0   = listArray (0,0) [coeffs!0]
  | otherwise = final
  where
    (0,n1) = bounds coeffs
    n      = n1 + 1
    hn     = div n 2
    hn1    = hn - 1
    hsub   = halveMulSubgroup subgroup
    g      = mulSubgroupGen   subgroup
    v_even = elems $ ntt hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i  ) | i<-[0..hn1] ]
    v_odd  = elems $ ntt hsub $ Poly $ listArray (0,hn1) [ coeffs!(2*i+1) | i<-[0..hn1] ]
    gpows  = powers hn g
    first  = zipWith3 (\gk x y -> (x + gk * y)) gpows v_even v_odd
    second = zipWith3 (\gk x y -> (x - gk * y)) gpows v_even v_odd
    final  = listArray (0,n1) (first ++ second) 

-- | Interpolates values into a polynomial on a subgroup
intt :: Field f => MulSubgroup f -> Array Int f -> Poly f
intt subgroup values 
  | n1+1 /= mulSubgroupOrder subgroup = error "intt: input size does not match the subgroup order"
  | n1 == 0   = Poly $ listArray (0,0) [values!0]
  | otherwise = final
  where
    (0,n1) = bounds values
    n      = n1 + 1
    hn     = div n 2
    hn1    = hn - 1
    hsub   = halveMulSubgroup subgroup
    g      = mulSubgroupGen   subgroup
    first  = [ values!(i   ) | i<-[0..hn1] ]
    second = [ values!(i+hn) | i<-[0..hn1] ]
    gpows  = powers hn g
    v_even = zipWith  (\  x y -> (x + y) /  2   )       first second
    v_odd  = zipWith3 (\g x y -> (x - y) / (2*g)) gpows first second
    p_even = elems $ polyCoeffArray $ intt hsub $ listArray (0,hn1) v_even
    p_odd  = elems $ polyCoeffArray $ intt hsub $ listArray (0,hn1) v_odd
    final  = Poly $ listArray (0,n1) (interleave p_even p_odd) 

--------------------------------------------------------------------------------

{-
-- manual testing

subgroup1 :: Subgroup Fermat8
subgroup1 = Subgroup { subgroupGen = pow mulGen_ 256 , subgroupOrder = 1 }

subgroup2 :: Subgroup Fermat8
subgroup2 = Subgroup { subgroupGen = pow mulGen_ 128 , subgroupOrder = 2 }

testpoly = Poly $ listArray (0,7) [11,13,17,19, 23,29,242,199]
values   = ntt subgroup8 testpoly
sanity   = [ polyEvalAt testpoly (pow (subgroupGen subgroup8) k) | k<-[0..7] ]
ok_ntt   = sanity == elems values

testvals = listArray (0,7) [11,13,17,19, 23,29,242,199]
poly     = intt subgroup8 testvals
reconstr = [ polyEvalAt poly (pow (subgroupGen subgroup8) k) | k<-[0..7] ]
ok_intt  = elems testvals == reconstr
-}

--------------------------------------------------------------------------------

