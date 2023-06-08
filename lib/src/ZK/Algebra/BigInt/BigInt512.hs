
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt512 where

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

newtype BigInt512 = MkBigInt512 (ForeignPtr Word64)
newtype BigInt1024 = MkBigInt1024 (ForeignPtr Word64)
newtype BigInt576 = MkBigInt576 (ForeignPtr Word64)

zero, one, two :: BigInt512
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt512 where
  (==) = isEqual

instance Num BigInt512 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt512 where
  show = show . from


----------------------------------------

foreign import ccall unsafe "bigint512_sqr" c_bigint512_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt512 -> BigInt1024
sqrExt (MkBigInt512 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_sqr ptr1 ptr2
  return (MkBigInt1024 fptr2)

foreign import ccall unsafe "bigint512_mul" c_bigint512_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt512 -> BigInt512 -> BigInt1024
mulExt (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint512_mul ptr1 ptr2 ptr3
  return (MkBigInt1024 fptr3)

foreign import ccall unsafe "bigint512_scale" c_bigint512_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt512 -> BigInt576
scaleExt s (MkBigInt512 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_scale s ptr1 ptr2
  return (MkBigInt576 fptr2)

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
mk :: Integer -> IO BigInt512
mk x = do
  fptr <- mallocForeignPtrArray 8
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 8 x
  return $ MkBigInt512 fptr

{-# NOINLINE get #-}
get :: BigInt512 -> IO Integer
get (MkBigInt512 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 8 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE to #-}
to :: Integer -> BigInt512
to x = unsafePerformIO (mk x)

{-# NOINLINE from #-}
from :: BigInt512 -> Integer
from f = unsafePerformIO (get f)

foreign import ccall unsafe "bigint512_is_zero" c_bigint512_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt512 -> Bool
isZero (MkBigInt512 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint512_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint512_is_one" c_bigint512_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt512 -> Bool
isOne (MkBigInt512 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint512_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint512_is_equal" c_bigint512_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt512 -> BigInt512 -> Bool
isEqual (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr1 $ \ptr2 -> do
      c_bigint512_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint512_set_small" c_bigint512_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt512
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint512_set_small ptr1 x
  return (MkBigInt512 fptr1)

foreign import ccall unsafe "bigint512_neg" c_bigint512_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt512 -> BigInt512
neg (MkBigInt512 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_neg ptr1 ptr2
  return (MkBigInt512 fptr2)

foreign import ccall unsafe "bigint512_add" c_bigint512_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt512 -> BigInt512 -> BigInt512
add (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint512_add ptr1 ptr2 ptr3
  return (MkBigInt512 fptr3)

foreign import ccall unsafe "bigint512_sub" c_bigint512_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt512 -> BigInt512 -> BigInt512
sub (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint512_sub ptr1 ptr2 ptr3
  return (MkBigInt512 fptr3)

foreign import ccall unsafe "bigint512_sqr_truncated" c_bigint512_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt512 -> BigInt512 -> BigInt512
sqr (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint512_sqr_truncated ptr1 ptr2 ptr3
  return (MkBigInt512 fptr3)

foreign import ccall unsafe "bigint512_mul_truncated" c_bigint512_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt512 -> BigInt512 -> BigInt512
mul (MkBigInt512 fptr1) (MkBigInt512 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint512_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt512 fptr3)

foreign import ccall unsafe "bigint512_shift_left_by_1" c_bigint512_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt512 -> (BigInt512, Bool)
shiftLeft1 (MkBigInt512 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_shift_left_by_1 ptr1 ptr2
  return (MkBigInt512 fptr2, cret /=0)

foreign import ccall unsafe "bigint512_shift_right_by_1" c_bigint512_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt512 -> (BigInt512, Bool)
shiftRight1 (MkBigInt512 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_shift_right_by_1 ptr1 ptr2
  return (MkBigInt512 fptr2, cret /=0)

foreign import ccall unsafe "bigint512_shift_left_by_k" c_bigint512_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt512 -> Int -> BigInt512
shiftLeft (MkBigInt512 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt512 fptr2)

foreign import ccall unsafe "bigint512_shift_right_by_k" c_bigint512_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt512 -> Int -> BigInt512
shiftRight (MkBigInt512 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint512_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt512 fptr2)
