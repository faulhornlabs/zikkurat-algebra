
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.Curves.BLS12_381.Mont.Fr
  ( Fr(..)
  , prime
  , to , from
  , toStd , fromStd
  , zero , one , two
  , isZero , isOne , isEqual
  , neg , add , sub
  , sqr , mul
  , inv , div
  , pow , pow_
  )
  where

--------------------------------------------------------------------------------

import Prelude  hiding (div)
import GHC.Real hiding (div)

import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import ZK.Algebra.BigInt.BigInt256( BigInt256(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Std.Fr as Std

--------------------------------------------------------------------------------  

newtype Fr = MkFr (ForeignPtr Word64)

prime :: Integer
prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

to :: Integer -> Fr
to = fromStd . Std.to

from :: Fr -> Integer
from = Std.from . toStd

zero, one, two :: Fr
zero = to 0
one  = to 1
two  = to 2

instance Eq Fr where
  (==) = isEqual

instance Num Fr where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Fractional Fr where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  (/)   = div

instance Show Fr where
  show = show . from

----------------------------------------

foreign import ccall unsafe "bls12_381_r_mont_from_std" c_bls12_381_r_mont_from_std :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE fromStd#-}
fromStd :: Std.Fr -> Fr
fromStd (Std.MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_from_std ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bls12_381_r_mont_to_std" c_bls12_381_r_mont_to_std :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE toStd#-}
toStd :: Fr -> Std.Fr
toStd (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_to_std ptr1 ptr2
  return (Std.MkFr fptr2)

foreign import ccall unsafe "bls12_381_r_mont_pow_gen" c_bls12_381_r_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow #-}
pow :: Fr -> BigInt256 -> Fr
pow (MkFr fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_r_mont_pow_gen ptr1 ptr2 ptr3
  return (MkFr fptr3)

----------------------------------------

fromWord64sLE :: [Word64] -> Integer
fromWord64sLE = go where
  go []     = 0
  go (x:xs) = fromIntegral x + shiftL (go xs) 64

toWord64sLE :: Integer -> [Word64]
toWord64sLE = go where
  go 0 = []
  go k = fromInteger (k .&. (2^64-1)) : go (shiftR k 64)

toWord64sLE' :: Int -> Integer -> [Word64]
toWord64sLE' len what = take len $ toWord64sLE what ++ repeat 0

foreign import ccall unsafe "bigint256_is_zero" c_bigint256_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fr -> Bool
isZero (MkFr fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_one" c_bigint256_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fr -> Bool
isOne (MkFr fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_equal" c_bigint256_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fr -> Fr -> Bool
isEqual (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr1 $ \ptr2 -> do
      c_bigint256_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_r_mont_neg" c_bls12_381_r_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fr -> Fr
neg (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_neg ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bls12_381_r_mont_add" c_bls12_381_r_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fr -> Fr -> Fr
add (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_r_mont_add ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bls12_381_r_mont_sub" c_bls12_381_r_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fr -> Fr -> Fr
sub (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_r_mont_sub ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bls12_381_r_mont_sqr" c_bls12_381_r_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fr -> Fr
sqr (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_sqr ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bls12_381_r_mont_mul" c_bls12_381_r_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fr -> Fr -> Fr
mul (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_r_mont_mul ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bls12_381_r_mont_inv" c_bls12_381_r_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fr -> Fr
inv (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_inv ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bls12_381_r_mont_div" c_bls12_381_r_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fr -> Fr -> Fr
div (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_r_mont_div ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bls12_381_r_mont_pow_uint64" c_bls12_381_r_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fr -> Word64 -> Fr
pow_ (MkFr fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_r_mont_pow_uint64 ptr1 x ptr2
  return (MkFr fptr2)
