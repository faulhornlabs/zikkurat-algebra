
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt256
  ( BigInt256(..)
  , to
  , from
  , small , zero , one , two
  , isZero , isOne , isEqual
  , neg , add , sub
  , sqr , mul
  , shiftLeft1 , shiftRight1
  , shiftLeft  , shiftRight
  , sqrExt
  , mulExt
  , scaleExt
  )
  where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import ZK.Algebra.BigInt.Types

--------------------------------------------------------------------------------  

to :: Integer -> BigInt256
to = unsafeTo

zero, one, two :: BigInt256
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt256 where
  (==) = isEqual

instance Num BigInt256 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt256 where
  show = show . from


----------------------------------------

foreign import ccall unsafe "bigint256_sqr" c_bigint256_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt256 -> BigInt512
sqrExt (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_sqr ptr1 ptr2
  return (MkBigInt512 fptr2)

foreign import ccall unsafe "bigint256_mul" c_bigint256_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt256 -> BigInt256 -> BigInt512
mulExt (MkBigInt256 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint256_mul ptr1 ptr2 ptr3
  return (MkBigInt512 fptr3)

foreign import ccall unsafe "bigint256_scale" c_bigint256_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt256 -> BigInt320
scaleExt s (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_scale s ptr1 ptr2
  return (MkBigInt320 fptr2)

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

{-# NOINLINE unsafeMk #-}
unsafeMk :: Integer -> IO BigInt256
unsafeMk x = do
  fptr <- mallocForeignPtrArray 4
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 4 x
  return $ MkBigInt256 fptr

{-# NOINLINE get #-}
get :: BigInt256 -> IO Integer
get (MkBigInt256 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 4 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt256
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE from #-}
from :: BigInt256 -> Integer
from f = unsafePerformIO (get f)

foreign import ccall unsafe "bigint256_is_zero" c_bigint256_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt256 -> Bool
isZero (MkBigInt256 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_one" c_bigint256_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt256 -> Bool
isOne (MkBigInt256 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_equal" c_bigint256_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt256 -> BigInt256 -> Bool
isEqual (MkBigInt256 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr1 $ \ptr2 -> do
      c_bigint256_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint256_set_small" c_bigint256_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt256
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint256_set_small ptr1 x
  return (MkBigInt256 fptr1)

foreign import ccall unsafe "bigint256_neg" c_bigint256_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt256 -> BigInt256
neg (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_neg ptr1 ptr2
  return (MkBigInt256 fptr2)

foreign import ccall unsafe "bigint256_add" c_bigint256_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt256 -> BigInt256 -> BigInt256
add (MkBigInt256 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint256_add ptr1 ptr2 ptr3
  return (MkBigInt256 fptr3)

foreign import ccall unsafe "bigint256_sub" c_bigint256_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt256 -> BigInt256 -> BigInt256
sub (MkBigInt256 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint256_sub ptr1 ptr2 ptr3
  return (MkBigInt256 fptr3)

foreign import ccall unsafe "bigint256_sqr_truncated" c_bigint256_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt256 -> BigInt256
sqr (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_sqr_truncated ptr1 ptr2
  return (MkBigInt256 fptr2)

foreign import ccall unsafe "bigint256_mul_truncated" c_bigint256_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt256 -> BigInt256 -> BigInt256
mul (MkBigInt256 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint256_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt256 fptr3)

foreign import ccall unsafe "bigint256_shift_left_by_1" c_bigint256_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt256 -> (BigInt256, Bool)
shiftLeft1 (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_shift_left_by_1 ptr1 ptr2
  return (MkBigInt256 fptr2, cret /=0)

foreign import ccall unsafe "bigint256_shift_right_by_1" c_bigint256_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt256 -> (BigInt256, Bool)
shiftRight1 (MkBigInt256 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_shift_right_by_1 ptr1 ptr2
  return (MkBigInt256 fptr2, cret /=0)

foreign import ccall unsafe "bigint256_shift_left_by_k" c_bigint256_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt256 -> Int -> BigInt256
shiftLeft (MkBigInt256 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt256 fptr2)

foreign import ccall unsafe "bigint256_shift_right_by_k" c_bigint256_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt256 -> Int -> BigInt256
shiftRight (MkBigInt256 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt256 fptr2)
