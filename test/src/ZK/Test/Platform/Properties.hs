
-- | Tests for platform-specific code (eg. addition with carry)

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Platform.Properties where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Proxy

import Control.Monad

import System.Random
import System.IO

import ZK.Algebra.BigInt.Platform

--------------------------------------------------------------------------------

runPlatformTests :: IO ()
runPlatformTests = do

  checkAll "addcarry_u64" 
    [ refAddCarry64 c x y == addCarry64 c x y 
    | x <- rangeBig
    , y <- rangeBig
    , c <- rangeCarry
    ]

  checkAll "subcarry_u64" 
    [ refSubBorrow64 b x y == subBorrow64 b x y 
    | x <- rangeBig
    , y <- rangeBig
    , b <- rangeCarry
    ]

  checkAll "addcarry_u128" 
    [ refAddCarry128 (xlo,xhi) (ylo,yhi) == addCarry128 (xlo,xhi) (ylo,yhi)
    | xlo <- rangeSmall
    , xhi <- rangeSmall
    , ylo <- rangeSmall
    , yhi <- rangeSmall
    ]

--------------------------------------------------------------------------------

checkAll :: String -> [Bool] -> IO ()
checkAll name oks = do
  let str = " - " ++ name ++ "... " 
  putStr $ str ++ replicate (30 - length str) ' '
  let n = length oks
  case and oks of 
    True  -> putStrLn $ "ok (passed " ++ show n ++ " non-randomized tests)"
    False -> putStrLn $ "FAILED!! (FAILED " ++ show (countFalses oks) ++ " tests!)"

countFalses :: [Bool] -> Int
countFalses = length . filter (==False)

--------------------------------------------------------------------------------

rangeCarry :: [Bool]
rangeCarry = [False,True]

rangeSmall :: [Word64]
rangeSmall = map fromInteger [-10..10]

rangeBig :: [Word64]
rangeBig = map fromInteger [-100..100]

--------------------------------------------------------------------------------

refAddCarry64 :: Bool -> Word64 -> Word64 -> (Bool,Word64)
refAddCarry64 c x y = (carry,out) where
  result :: Integer
  result = fromIntegral x + fromIntegral y + (if c then 1 else 0)
  carry = (shiftR result 64 /= 0)
  out   = fromInteger result

refSubBorrow64 :: Bool -> Word64 -> Word64 -> (Bool,Word64)
refSubBorrow64 b x y = (carry,out) where
  result :: Integer
  result = fromIntegral x - fromIntegral y - (if b then 1 else 0)
  carry = result < 0 
  out   = fromInteger result

refAddCarry128 :: (Word64,Word64) -> (Word64,Word64) -> (Bool,(Word64,Word64))
refAddCarry128 (xlo,xhi) (ylo,yhi) = (carry,(outLo,outHi)) where
  x, y :: Integer
  x = fromIntegral xlo + shiftL (fromIntegral xhi) 64 
  y = fromIntegral ylo + shiftL (fromIntegral yhi) 64 
  result :: Integer
  result = fromIntegral x + fromIntegral y 
  carry = (shiftR result 128 /= 0)
  outLo = fromInteger         result     :: Word64
  outHi = fromInteger (shiftR result 64) :: Word64

--------------------------------------------------------------------------------
