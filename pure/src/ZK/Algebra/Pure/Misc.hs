
-- | Helper functions

{-# LANGUAGE NumericUnderscores #-}
module ZK.Algebra.Pure.Misc where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word
import Data.Proxy
import Numeric.Natural

import System.Random

import Debug.Trace

--------------------------------------------------------------------------------
-- * debugging

debug :: Show a => String -> a -> b -> b 
debug msg x y = trace (">>> " ++ msg ++ " = " ++ show x) y 

--------------------------------------------------------------------------------
-- * type classes (TODO: move these somewhere else?)

class Rnd a where
  rndIO :: IO a

class Serialize a where
  sizeInWords            :: Proxy a -> Int
  toWords                :: a -> [Word64]
  parseWords             :: [Word64] -> Maybe (a,[Word64])
  mbFromWords            :: [Word64] -> Maybe a

  mbFromWords ws = case parseWords ws of
    Just (x,rest) -> if null rest then Just x else Nothing
    Nothing       -> Nothing

  parseWords  ws = mb where
    pxy = proxyOfLeft1 mb
    n   = sizeInWords pxy
    (this,rest) = splitAt n ws
    mb  = case mbFromWords this of
      Just y  -> Just (y,rest) 
      Nothing -> Nothing

class Serialize a => SerializeMontgomery a where
  toWordsMontgomery      :: a -> [Word64]
  parseWordsMontgomery   :: [Word64] -> Maybe (a,[Word64])
  mbFromWordsMontgomery  :: [Word64] -> Maybe a

  mbFromWordsMontgomery ws = case parseWordsMontgomery ws of
    Just (x,rest) -> if null rest then Just x else Nothing
    Nothing       -> Nothing

  parseWordsMontgomery ws = mb where
    pxy = proxyOfLeft1 mb
    n   = sizeInWords pxy
    (this,rest) = splitAt n ws
    mb  = case mbFromWordsMontgomery this of
      Just y  -> Just (y,rest) 
      Nothing -> Nothing

fromWords :: Serialize a => [Word64] -> a
fromWords ws = case mbFromWords ws of { Just y -> y ; Nothing -> error "fromWords: invalid" }

fromWordsMontgomery :: SerializeMontgomery a => [Word64] -> a
fromWordsMontgomery ws = case mbFromWordsMontgomery ws of { Just y -> y ; Nothing -> error "fromWordsMontgomery: invalid" }

--------------------------------------------------------------------------------

-- | Minimum number of 64-bit words which can store this (positive) integer
integerRequiredNumberOfWords :: Integer -> Int
integerRequiredNumberOfWords n = (ceilingLog2 n + 63) `div` 64

integerToWordsLE :: Int -> Integer -> [Word64]
integerToWordsLE !n !a = go n a where
  go  0 !a  = if a==0 then [] else error "integerToWordsLE: does not fit in the given size"
  go !k !a  = (fromInteger $ a .&. 0xffff_ffff_ffff_ffff) : go (k-1) (shiftR a 64)

integerFromWordsLE :: [Word64] -> Integer
integerFromWordsLE = go where
  go []      = 0
  go (!w:ws) = fromIntegral w + shiftL (go ws) 64

--------------------------------------------------------------------------------
-- * integers

integerToNatural :: Integer -> Natural
integerToNatural k
  | k < 0     = error "integerToNatural: negative"
  | otherwise = fromInteger k

naturalToInteger :: Natural -> Integer
naturalToInteger = fromIntegral

-- | Smallest integer @k@ such that @2^k@ is larger or equal to @n@
ceilingLog2 :: Integer -> Int
ceilingLog2 0 = 0
ceilingLog2 n = 1 + go (n-1) where
  go 0 = -1
  go k = 1 + go (shiftR k 1)

isEven :: Integer -> Bool
isEven n = (n .&. 1) == 0 

isOdd :: Integer -> Bool
isOdd n = (n .&. 1) /= 0 

-- div2, mod2 :: Integer -> Integer
-- div2 n = shiftR n 1
-- mod2 n = n .&. 1

div2, div4, div8, div16 :: Integer -> Integer
div2  n = shiftR n 1
div4  n = shiftR n 2
div8  n = shiftR n 3
div16 n = shiftR n 4

mod2, mod4, mod8, mod16 :: Integer -> Integer
mod2  n = n .&. 1
mod4  n = n .&. 3
mod8  n = n .&. 7
mod16 n = n .&. 15

boolMod2 :: Integer -> Bool
boolMod2 n = (n .&. 1) /= 0

--------------------------------------------------------------------------------
-- * power-of-two decompositions

-- | The decomposition of a positive number as a product of a power of two and an odd number
data PowerOfTwoDecomp = POTD 
  { _potdExponent :: Int 
  , _potdOddPart  :: Integer 
  }
  deriving (Eq,Show)

powerOfTwoDecomp :: Integer -> PowerOfTwoDecomp
powerOfTwoDecomp = go 0 where
  go :: Int -> Integer -> PowerOfTwoDecomp
  go k n = if isEven n 
    then go (k+1) (div2 n)
    else POTD k n

--------------------------------------------------------------------------------
-- * lists

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave []     []     = []
interleave _      _      = error "interleave: expecting input lists of the same length"

----------------------------------------

partitionIntoChunks :: Int -> [a] -> [[a]]
partitionIntoChunks k = go where
  go [] = []
  go xs = take k xs : go (drop k xs)

--------------------------------------------------------------------------------
-- * arrays

-- | Returns the default value when out of range
safeIndex :: a -> Array Int a -> Int -> a
safeIndex def arr j 
  | j < a      = def
  | j > b      = def
  | otherwise  = arr!j
  where
    (a,b) = bounds arr

--------------------------------------------------------------------------------
-- * binary numbers

toBitsLE :: Integer -> [Bool]
toBitsLE 0 = []
toBitsLE n = ((n .&. 1) > 0) : toBitsLE (shiftR n 1)

toBitsBE :: Integer -> [Bool]
toBitsBE = reverse . toBitsLE

-- | skip the most significant bit because it's always 1 (for positive numbers, that is)
toBitsBE_ :: Integer -> [Bool]
toBitsBE_ n = case toBitsBE n of
  (True:bs) -> bs
  _         -> error "toBitsBE_: most significant bit is not 1"

--------------------------------------------------------------------------------
-- * random

condRandomIO :: Random a => (a -> Bool) -> IO a
condRandomIO cond = loop where
  loop = do
    x <- randomIO
    if cond x then return x else loop

--------------------------------------------------------------------------------
-- * Proxy

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

proxyOf1 :: f a -> Proxy a
proxyOf1 _ = Proxy

proxyOf2 :: g (f a) -> Proxy a
proxyOf2 _ = Proxy

proxyOfLeft :: (a,b) -> Proxy a
proxyOfLeft _ = Proxy

proxyOfLeft1 :: f (a,b) -> Proxy a
proxyOfLeft1 _ = Proxy

--------------------------------------------------------------------------------
