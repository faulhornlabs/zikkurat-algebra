
-- | Some concrete primes

module Zikkurat.Primes where

--------------------------------------------------------------------------------

import Data.Bits

--------------------------------------------------------------------------------
-- * Primes

type Prime = Integer

allPrimes :: [Prime]
allPrimes = snarkPrimes ++ testPrimes

snarkPrimes :: [Prime]
snarkPrimes =
  [ goldilocks_p
    --
  , bls12_381_base_p , bls12_381_scalar_r
  , bn128_base_p     , bn128_scalar_r
  ]

testPrimes :: [Prime]
testPrimes =
  [ prime_32 
  , prime_64 
  , prime_128
  , prime_160
  , prime_192
  , prime_224
  , prime_256
    -- 
  , prime_33 
  , prime_65 
  , prime_129
  , prime_161
  , prime_193
  , prime_225
  , prime_257  
  ]

--------------------------------------------------------------------------------
-- * test primes

-- primes closest to a given bit length
-- useful for testing corner cases

prime_32  = 2^32  -   5 :: Prime
prime_64  = 2^64  -  59 :: Prime
prime_128 = 2^128 - 159 :: Prime 
prime_160 = 2^160 -  47 :: Prime
prime_192 = 2^192 - 237 :: Prime
prime_224 = 2^224 -  63 :: Prime
prime_256 = 2^256 - 189 :: Prime

prime_33  = 2^32  +  15 :: Prime
prime_65  = 2^64  +  13 :: Prime
prime_129 = 2^128 +  51 :: Prime 
prime_161 = 2^160 +   7 :: Prime
prime_193 = 2^192 + 133 :: Prime
prime_225 = 2^224 + 735 :: Prime
prime_257 = 2^256 + 297 :: Prime

--------------------------------------------------------------------------------
-- * snark primes

goldilocks_p = 2^64 - 2^32 + 1 :: Prime

--------------------------------------------------------------------------------
-- BLS12-381 elliptic curve

bls12_381_base_p   = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab :: Prime
bls12_381_scalar_r = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001 :: Prime

--------------------------------------------------------------------------------
-- BN128 elliptic curve

bn128_base_p   = 21888242871839275222246405745257275088696311157297823662689037894645226208583 :: Prime
bn128_scalar_r = 21888242871839275222246405745257275088548364400416034343698204186575808495617 :: Prime

--------------------------------------------------------------------------------
-- * some useful functions

-- | Compute how many 64-bit limbs are needed to represent a field element
nlimbsRequired :: Prime -> Int
nlimbsRequired p = fromIntegral $ (ceilingLog2 p + 63) `div` 64 

--------------------------------------------------------------------------------
-- ** Integer logarithm

-- | Largest integer @k@ such that @2^k@ is smaller or equal to @n@
integerLog2 :: Integer -> Integer
integerLog2 n = go n where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Integer
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)
  
--------------------------------------------------------------------------------
-- ** Integer square root

isSquare :: Integer -> Bool
isSquare n = 
  if (fromIntegral $ mod n 32) `elem` rs 
    then snd (integerSquareRoot' n) == 0
    else False
  where
    rs = [0,1,4,9,16,17,25] :: [Int]
    
-- | Integer square root (largest integer whose square is smaller or equal to the input)
-- using Newton's method, with a faster (for large numbers) inital guess based on bit shifts.
integerSquareRoot :: Integer -> Integer
integerSquareRoot = fst . integerSquareRoot'

-- | Smallest integer whose square is larger or equal to the input
ceilingSquareRoot :: Integer -> Integer
ceilingSquareRoot n = (if r>0 then u+1 else u) where (u,r) = integerSquareRoot' n 

-- | We also return the excess residue; that is
--
-- > (a,r) = integerSquareRoot' n
-- 
-- means that
--
-- > a*a + r = n
-- > a*a <= n < (a+1)*(a+1)
integerSquareRoot' :: Integer -> (Integer,Integer)
integerSquareRoot' n
  | n<0 = error "integerSquareRoot: negative input"
  | n<2 = (n,0)
  | otherwise = go firstGuess 
  where
    k = integerLog2 n
    firstGuess = 2^(div (k+2) 2) -- !! note that (div (k+1) 2) is NOT enough !!
    go a = 
      if m < a
        then go a' 
        else (a, r + a*(m-a))
      where
        (m,r) = divMod n a
        a' = div (m + a) 2

-- | Newton's method without an initial guess. For very small numbers (<10^10) it
-- is somewhat faster than the above version.
integerSquareRootNewton' :: Integer -> (Integer,Integer)
integerSquareRootNewton' n
  | n<0 = error "integerSquareRootNewton: negative input"
  | n<2 = (n,0)
  | otherwise = go (div n 2) 
  where
    go a = 
      if m < a
        then go a' 
        else (a, r + a*(m-a))
      where
        (m,r) = divMod n a
        a' = div (m + a) 2

{-
-- brute force test of integer square root
isqrt_test n1 n2 = 
  [ k 
  | k<-[n1..n2] 
  , let (a,r) = integerSquareRoot' k
  , (a*a+r/=k) || (a*a>k) || (a+1)*(a+1)<=k 
  ]
-}

--------------------------------------------------------------------------------

