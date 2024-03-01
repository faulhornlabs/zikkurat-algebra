
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module ZK.Algebra.Class.Misc where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.List
  
--------------------------------------------------------------------------------
-- * Logarithms

-- | Base two logarithm of an integer (which is a power of two)
newtype Log2 
  = Log2 Int
  deriving (Eq,Ord,Show,Num)

fromLog2 :: Log2 -> Int
fromLog2 (Log2 k) = k

exp2 :: Log2 -> Integer
exp2 (Log2 e) = shiftL 1 e   -- 2^e

exp2_ :: Log2 -> Int
exp2_ (Log2 e) = shiftL 1 e   -- 2^e

-- | Largest integer @k@ such that @2^k@ is smaller or equal to @n@
integerLog2 :: Integer -> Log2 
integerLog2 n = Log2 (go n) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Log2 
ceilingLog2 0 = Log2 $ 0
ceilingLog2 n = Log2 $ 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

integerLog2_ :: Int -> Log2 
integerLog2_ = integerLog2 . fromIntegral

ceilingLog2_ :: Int -> Log2 
ceilingLog2_ = ceilingLog2 . fromIntegral

--------------------------------------------------------------------------------
-- * Random

class Rnd a where
  rndIO :: IO a

--------------------------------------------------------------------------------
