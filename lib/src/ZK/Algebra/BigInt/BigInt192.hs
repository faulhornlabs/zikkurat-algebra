
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt192
  ( BigInt192(..)
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

to :: Integer -> BigInt192
to = unsafeTo

zero, one, two :: BigInt192
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt192 where
  (==) = isEqual

instance Num BigInt192 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt192 where
  show = show . from


----------------------------------------

foreign import ccall unsafe "bigint192_sqr" c_bigint192_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt192 -> BigInt384
sqrExt (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_sqr ptr1 ptr2
  return (MkBigInt384 fptr2)

foreign import ccall unsafe "bigint192_mul" c_bigint192_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt192 -> BigInt192 -> BigInt384
mulExt (MkBigInt192 fptr1) (MkBigInt192 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint192_mul ptr1 ptr2 ptr3
  return (MkBigInt384 fptr3)

foreign import ccall unsafe "bigint192_scale" c_bigint192_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt192 -> BigInt256
scaleExt s (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_scale s ptr1 ptr2
  return (MkBigInt256 fptr2)

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
unsafeMk :: Integer -> IO BigInt192
unsafeMk x = do
  fptr <- mallocForeignPtrArray 3
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 3 x
  return $ MkBigInt192 fptr

{-# NOINLINE get #-}
get :: BigInt192 -> IO Integer
get (MkBigInt192 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 3 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt192
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE from #-}
from :: BigInt192 -> Integer
from f = unsafePerformIO (get f)

foreign import ccall unsafe "bigint192_is_zero" c_bigint192_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt192 -> Bool
isZero (MkBigInt192 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint192_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint192_is_one" c_bigint192_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt192 -> Bool
isOne (MkBigInt192 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint192_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint192_is_equal" c_bigint192_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt192 -> BigInt192 -> Bool
isEqual (MkBigInt192 fptr1) (MkBigInt192 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr1 $ \ptr2 -> do
      c_bigint192_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint192_set_small" c_bigint192_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt192
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint192_set_small ptr1 x
  return (MkBigInt192 fptr1)

foreign import ccall unsafe "bigint192_neg" c_bigint192_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt192 -> BigInt192
neg (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_neg ptr1 ptr2
  return (MkBigInt192 fptr2)

foreign import ccall unsafe "bigint192_add" c_bigint192_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt192 -> BigInt192 -> BigInt192
add (MkBigInt192 fptr1) (MkBigInt192 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint192_add ptr1 ptr2 ptr3
  return (MkBigInt192 fptr3)

foreign import ccall unsafe "bigint192_sub" c_bigint192_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt192 -> BigInt192 -> BigInt192
sub (MkBigInt192 fptr1) (MkBigInt192 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint192_sub ptr1 ptr2 ptr3
  return (MkBigInt192 fptr3)

foreign import ccall unsafe "bigint192_sqr_truncated" c_bigint192_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt192 -> BigInt192
sqr (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_sqr_truncated ptr1 ptr2
  return (MkBigInt192 fptr2)

foreign import ccall unsafe "bigint192_mul_truncated" c_bigint192_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt192 -> BigInt192 -> BigInt192
mul (MkBigInt192 fptr1) (MkBigInt192 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint192_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt192 fptr3)

foreign import ccall unsafe "bigint192_shift_left_by_1" c_bigint192_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt192 -> (BigInt192, Bool)
shiftLeft1 (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_shift_left_by_1 ptr1 ptr2
  return (MkBigInt192 fptr2, cret /=0)

foreign import ccall unsafe "bigint192_shift_right_by_1" c_bigint192_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt192 -> (BigInt192, Bool)
shiftRight1 (MkBigInt192 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_shift_right_by_1 ptr1 ptr2
  return (MkBigInt192 fptr2, cret /=0)

foreign import ccall unsafe "bigint192_shift_left_by_k" c_bigint192_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt192 -> Int -> BigInt192
shiftLeft (MkBigInt192 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt192 fptr2)

foreign import ccall unsafe "bigint192_shift_right_by_k" c_bigint192_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt192 -> Int -> BigInt192
shiftRight (MkBigInt192 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 3
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint192_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt192 fptr2)
