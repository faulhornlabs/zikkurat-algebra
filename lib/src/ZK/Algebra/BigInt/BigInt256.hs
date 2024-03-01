
-- | 256-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt256
  ( BigInt256(..)
    -- * Conversion
  , to
  , from
    -- * Some numbers
  , small , zero , one , two
    -- * Predicates
  , isZero , isOne , isEqual
    -- * Ring operations
  , neg , add , sub
  , sqr , mul
    -- * Shifts
  , shiftLeft1 , shiftRight1
  , shiftLeft  , shiftRight
    -- * Extended multiplication
  , sqrExt
  , mulExt
  , scaleExt
    -- * Random
  , rnd
  )
  where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Control.Monad

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal

import System.Random
import System.IO.Unsafe

import ZK.Algebra.BigInt.Types

import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import qualified ZK.Algebra.Class.Misc  as M

--------------------------------------------------------------------------------  

to :: Integer -> BigInt256
to = unsafeTo

from :: BigInt256 -> Integer
from = unsafeFrom

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

instance L.Flat BigInt256 where
  sizeInBytes  _pxy = 32
  sizeInQWords _pxy = 4
  withFlat (MkBigInt256 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt256 4

rnd :: IO BigInt256
rnd = do
  fptr <- mallocForeignPtrArray 4
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 4 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt256 fptr)

instance M.Rnd BigInt256 where
  rndIO = rnd

instance C.Ring BigInt256 where
  ringName _ = "BigInt256"
  ringSize _ = 115792089237316195423570985008687907853269984665640564039457584007913129639936
  isZero = ZK.Algebra.BigInt.BigInt256.isZero
  isOne  = ZK.Algebra.BigInt.BigInt256.isOne
  zero   = ZK.Algebra.BigInt.BigInt256.zero
  one    = ZK.Algebra.BigInt.BigInt256.one
  square = ZK.Algebra.BigInt.BigInt256.sqr
  power  = C.ringPowerDefault

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

toWord64sLE_ :: Integer -> [Word64]
toWord64sLE_ = go where
  go 0 = []
  go k = fromInteger (k .&. (2^64-1)) : go (shiftR k 64)

toWord64sLE :: Int -> Integer -> [Word64]
toWord64sLE len what = take len $ toWord64sLE_ what ++ repeat 0

{-# NOINLINE unsafeMk #-}
unsafeMk :: Integer -> IO BigInt256
unsafeMk x = do
  fptr <- mallocForeignPtrArray 4
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 4 x
  return $ MkBigInt256 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt256 -> IO Integer
unsafeGet (MkBigInt256 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 4 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt256
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt256 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

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
    withForeignPtr fptr2 $ \ptr2 -> do
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
