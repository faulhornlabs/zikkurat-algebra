
-- | 320-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt320
  ( BigInt320(..)
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

to :: Integer -> BigInt320
to = unsafeTo

from :: BigInt320 -> Integer
from = unsafeFrom

zero, one, two :: BigInt320
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt320 where
  (==) = isEqual

instance Num BigInt320 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt320 where
  show = show . from

instance L.Flat BigInt320 where
  sizeInBytes  _pxy = 40
  sizeInQWords _pxy = 5
  withFlat (MkBigInt320 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt320 5

rnd :: IO BigInt320
rnd = do
  fptr <- mallocForeignPtrArray 5
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 5 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt320 fptr)

instance M.Rnd BigInt320 where
  rndIO = rnd

instance C.Ring BigInt320 where
  ringNamePxy _ = "BigInt320"
  ringSizePxy _ = 2135987035920910082395021706169552114602704522356652769947041607822219725780640550022962086936576
  isZero = ZK.Algebra.BigInt.BigInt320.isZero
  isOne  = ZK.Algebra.BigInt.BigInt320.isOne
  zero   = ZK.Algebra.BigInt.BigInt320.zero
  one    = ZK.Algebra.BigInt.BigInt320.one
  square = ZK.Algebra.BigInt.BigInt320.sqr
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint320_sqr" c_bigint320_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt320 -> BigInt640
sqrExt (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_sqr ptr1 ptr2
  return (MkBigInt640 fptr2)

foreign import ccall unsafe "bigint320_mul" c_bigint320_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt320 -> BigInt320 -> BigInt640
mulExt (MkBigInt320 fptr1) (MkBigInt320 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint320_mul ptr1 ptr2 ptr3
  return (MkBigInt640 fptr3)

foreign import ccall unsafe "bigint320_scale" c_bigint320_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt320 -> BigInt384
scaleExt s (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_scale s ptr1 ptr2
  return (MkBigInt384 fptr2)

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
unsafeMk :: Integer -> IO BigInt320
unsafeMk x = do
  fptr <- mallocForeignPtrArray 5
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 5 x
  return $ MkBigInt320 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt320 -> IO Integer
unsafeGet (MkBigInt320 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 5 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt320
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt320 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint320_is_zero" c_bigint320_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt320 -> Bool
isZero (MkBigInt320 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint320_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint320_is_one" c_bigint320_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt320 -> Bool
isOne (MkBigInt320 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint320_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint320_is_equal" c_bigint320_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt320 -> BigInt320 -> Bool
isEqual (MkBigInt320 fptr1) (MkBigInt320 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint320_set_small" c_bigint320_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt320
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint320_set_small ptr1 x
  return (MkBigInt320 fptr1)

foreign import ccall unsafe "bigint320_neg" c_bigint320_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt320 -> BigInt320
neg (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_neg ptr1 ptr2
  return (MkBigInt320 fptr2)

foreign import ccall unsafe "bigint320_add" c_bigint320_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt320 -> BigInt320 -> BigInt320
add (MkBigInt320 fptr1) (MkBigInt320 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint320_add ptr1 ptr2 ptr3
  return (MkBigInt320 fptr3)

foreign import ccall unsafe "bigint320_sub" c_bigint320_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt320 -> BigInt320 -> BigInt320
sub (MkBigInt320 fptr1) (MkBigInt320 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint320_sub ptr1 ptr2 ptr3
  return (MkBigInt320 fptr3)

foreign import ccall unsafe "bigint320_sqr_truncated" c_bigint320_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt320 -> BigInt320
sqr (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_sqr_truncated ptr1 ptr2
  return (MkBigInt320 fptr2)

foreign import ccall unsafe "bigint320_mul_truncated" c_bigint320_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt320 -> BigInt320 -> BigInt320
mul (MkBigInt320 fptr1) (MkBigInt320 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint320_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt320 fptr3)

foreign import ccall unsafe "bigint320_shift_left_by_1" c_bigint320_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt320 -> (BigInt320, Bool)
shiftLeft1 (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_shift_left_by_1 ptr1 ptr2
  return (MkBigInt320 fptr2, cret /=0)

foreign import ccall unsafe "bigint320_shift_right_by_1" c_bigint320_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt320 -> (BigInt320, Bool)
shiftRight1 (MkBigInt320 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_shift_right_by_1 ptr1 ptr2
  return (MkBigInt320 fptr2, cret /=0)

foreign import ccall unsafe "bigint320_shift_left_by_k" c_bigint320_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt320 -> Int -> BigInt320
shiftLeft (MkBigInt320 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt320 fptr2)

foreign import ccall unsafe "bigint320_shift_right_by_k" c_bigint320_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt320 -> Int -> BigInt320
shiftRight (MkBigInt320 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 5
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint320_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt320 fptr2)
