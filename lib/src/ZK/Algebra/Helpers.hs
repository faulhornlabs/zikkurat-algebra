
{-# LANGUAGE BangPatterns #-}
module ZK.Algebra.Helpers where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.List
import Text.Printf

--------------------------------------------------------------------------------
-- * Integers

happyMod :: Integer -> Integer -> Integer
happyMod !a !p 
  | a >= 0 && a < p   = a
  | otherwise         = mod a p

--------------------------------------------------------------------------------
-- * Export constants to C

-- | Prints a 64-bit word in hexadecimal, includeing the @"0x"@ prefix
showHex64 :: Word64 -> String
showHex64 = printf "0x%016x"

-- | Same as 'showHex64', except without the @"0x"@ prefix
showHex64_ :: Word64 -> String
showHex64_ = printf "%016x"

--------------------------------------------------------------------------------

-- | Convert from 64-bit words, little-endian
fromWord64sLE :: [Word64] -> Integer
fromWord64sLE = go where
  go []     = 0
  go (x:xs) = fromIntegral x + shiftL (go xs) 64

-- | Convert to @n@ 64-bit words, little-endian
toWord64sLE :: Int -> Integer -> [Word64]
toWord64sLE len what = take len $ toWord64sLE__ what ++ repeat 0

-- | Convert to the minimum required nunber of 64-bit words (returning the length), little-endian
toWord64sLE_ :: Integer -> (Int,[Word64])
toWord64sLE_ = go where
  go  0 = (0,[])
  go !k = fromInteger (k .&. (2^64-1)) `cons` go (shiftR k 64)
  cons !x (!l,!xs) = (l+1, x:xs)

-- | Convert to the minimum required nunber of 64-bit words, little-endian
toWord64sLE__ :: Integer -> [Word64]
toWord64sLE__ = go where
  go  0 = []
  go !k = fromInteger (k .&. (2^64-1)) : go (shiftR k 64)

--------------------------------------------------------------------------------
-- ** Export lists of words

-- | Outputs a C array of a list of 64-bit words
exportWordsToC_ :: [Word64] -> String 
exportWordsToC_ ws = "{ " ++ intercalate ", " (map showHex64 ws) ++ " }" where

-- | Outputs a C definition of a list of 64-bit words
exportWordsToC :: String -> [Word64] -> String 
exportWordsToC name ws = "const uint64_t " ++ name ++ "[" ++ show (length ws)  ++ "] = " ++ exportWordsToC_ ws ++ ";"

-- | Outputs a C definitaion of a large list of 64-bit words
exportWordsArrayToC :: Int -> String -> [Word64] -> String 
exportWordsArrayToC n name ws = text where
  text = "const uint64_t " ++ name ++ "[" ++ show (length ws) ++ "] =\n" ++
         concat (zipWith f seps wss) ++
         "  };" 
  f sep ws = "  " ++ [sep] ++ " " ++ intercalate ", " (map showHex64 $ ws) ++ "\n" 
  wss = partition $ ws
  partition [] = []
  partition xs = take n xs : partition (drop n xs)
  seps = '{' : repeat ','

--------------------------------------------------------------------------------
-- ** Export big integers

-- | Outputs a C array of @n@ 64-bit words
exportConstToC_ :: Int -> Integer -> String 
exportConstToC_ n value = exportWordsToC_ (toWord64sLE n value)

-- | Outputs a C definition of a const array of @n@ 64-bit words
exportConstToC :: Int -> String -> Integer -> String 
exportConstToC n name value = exportWordsToC name (toWord64sLE n value)

exportConstArrayToC :: Int -> String -> [Integer] -> String 
exportConstArrayToC n name values = text where
  text = exportWordsArrayToC n name ws
  ws   = concat $ map (toWord64sLE n) values

--------------------------------------------------------------------------------
