
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt128 where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

--------------------------------------------------------------------------------  

newtype BigInt128 = MkBigInt128 (ForeignPtr Word64)
newtype BigInt256 = MkBigInt256 (ForeignPtr Word64)
newtype BigInt192 = MkBigInt192 (ForeignPtr Word64)

zero, one, two :: BigInt128
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt128 where
  (==) = isEqual

instance Num BigInt128 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt128 where
  show = show . from


----------------------------------------

foreign import ccall unsafe "bigint128_sqr" c_bigint128_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt128 -> BigInt256
sqrExt (MkBigInt128 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_sqr ptr1 ptr2
  return (MkBigInt256 fptr2)

foreign import ccall unsafe "bigint128_mul" c_bigint128_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt128 -> BigInt128 -> BigInt256
mulExt (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint128_mul ptr1 ptr2 ptr3
  return (MkBigInt256 fptr3)

foreign import ccall unsafe "bigint128_scale" c_bigint128_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt128 -> BigInt192
scaleExt s (MkBigInt128 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_scale s ptr1 ptr2
  return (MkBigInt192 fptr2)

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

{-# NOINLINE mk #-}
mk :: Integer -> IO BigInt128
mk x = do
  fptr <- mallocForeignPtrArray 2
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 2 x
  return $ MkBigInt128 fptr

{-# NOINLINE get #-}
get :: BigInt128 -> IO Integer
get (MkBigInt128 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 2 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE to #-}
to :: Integer -> BigInt128
to x = unsafePerformIO (mk x)

{-# NOINLINE from #-}
from :: BigInt128 -> Integer
from f = unsafePerformIO (get f)

foreign import ccall unsafe "bigint128_is_zero" c_bigint128_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt128 -> Bool
isZero (MkBigInt128 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint128_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint128_is_one" c_bigint128_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt128 -> Bool
isOne (MkBigInt128 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint128_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint128_is_equal" c_bigint128_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt128 -> BigInt128 -> Bool
isEqual (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr1 $ \ptr2 -> do
      c_bigint128_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint128_set_small" c_bigint128_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt128
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint128_set_small ptr1 x
  return (MkBigInt128 fptr1)

foreign import ccall unsafe "bigint128_neg" c_bigint128_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt128 -> BigInt128
neg (MkBigInt128 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_neg ptr1 ptr2
  return (MkBigInt128 fptr2)

foreign import ccall unsafe "bigint128_add" c_bigint128_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt128 -> BigInt128 -> BigInt128
add (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint128_add ptr1 ptr2 ptr3
  return (MkBigInt128 fptr3)

foreign import ccall unsafe "bigint128_sub" c_bigint128_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt128 -> BigInt128 -> BigInt128
sub (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint128_sub ptr1 ptr2 ptr3
  return (MkBigInt128 fptr3)

foreign import ccall unsafe "bigint128_sqr_truncated" c_bigint128_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt128 -> BigInt128 -> BigInt128
sqr (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint128_sqr_truncated ptr1 ptr2 ptr3
  return (MkBigInt128 fptr3)

foreign import ccall unsafe "bigint128_mul_truncated" c_bigint128_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt128 -> BigInt128 -> BigInt128
mul (MkBigInt128 fptr1) (MkBigInt128 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint128_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt128 fptr3)

foreign import ccall unsafe "bigint128_shift_left_by_1" c_bigint128_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt128 -> (BigInt128, Bool)
shiftLeft1 (MkBigInt128 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 2
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_shift_left_by_1 ptr1 ptr2
  return (MkBigInt128 fptr2, cret /=0)

foreign import ccall unsafe "bigint128_shift_right_by_1" c_bigint128_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt128 -> (BigInt128, Bool)
shiftRight1 (MkBigInt128 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 2
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_shift_right_by_1 ptr1 ptr2
  return (MkBigInt128 fptr2, cret /=0)

foreign import ccall unsafe "bigint128_shift_left_by_k" c_bigint128_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt128 -> Int -> BigInt128
shiftLeft (MkBigInt128 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt128 fptr2)

foreign import ccall unsafe "bigint128_shift_right_by_k" c_bigint128_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt128 -> Int -> BigInt128
shiftRight (MkBigInt128 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 2
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint128_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt128 fptr2)
