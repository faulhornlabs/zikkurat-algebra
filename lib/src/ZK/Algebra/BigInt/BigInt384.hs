
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt384
  ( BigInt384(..)
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
import qualified ZK.Algebra.Class.Field as C

--------------------------------------------------------------------------------  

to :: Integer -> BigInt384
to = unsafeTo

from :: BigInt384 -> Integer
from = unsafeFrom

zero, one, two :: BigInt384
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt384 where
  (==) = isEqual

instance Num BigInt384 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt384 where
  show = show . from

rnd :: IO BigInt384
rnd = do
  fptr <- mallocForeignPtrArray 6
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 6 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt384 fptr)

instance C.Rnd BigInt384 where
  rndIO = rnd

instance C.Ring BigInt384 where
  ringNamePxy _ = "BigInt384"
  ringSizePxy _ = 39402006196394479212279040100143613805079739270465446667948293404245721771497210611414266254884915640806627990306816
  isZero = isZero
  isOne  = isOne
  zero   = zero
  one    = one
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint384_sqr" c_bigint384_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt384 -> BigInt768
sqrExt (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_sqr ptr1 ptr2
  return (MkBigInt768 fptr2)

foreign import ccall unsafe "bigint384_mul" c_bigint384_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt384 -> BigInt384 -> BigInt768
mulExt (MkBigInt384 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint384_mul ptr1 ptr2 ptr3
  return (MkBigInt768 fptr3)

foreign import ccall unsafe "bigint384_scale" c_bigint384_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt384 -> BigInt448
scaleExt s (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_scale s ptr1 ptr2
  return (MkBigInt448 fptr2)

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
unsafeMk :: Integer -> IO BigInt384
unsafeMk x = do
  fptr <- mallocForeignPtrArray 6
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 6 x
  return $ MkBigInt384 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt384 -> IO Integer
unsafeGet (MkBigInt384 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 6 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt384
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt384 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint384_is_zero" c_bigint384_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt384 -> Bool
isZero (MkBigInt384 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint384_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint384_is_one" c_bigint384_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt384 -> Bool
isOne (MkBigInt384 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint384_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint384_is_equal" c_bigint384_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt384 -> BigInt384 -> Bool
isEqual (MkBigInt384 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint384_set_small" c_bigint384_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt384
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint384_set_small ptr1 x
  return (MkBigInt384 fptr1)

foreign import ccall unsafe "bigint384_neg" c_bigint384_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt384 -> BigInt384
neg (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_neg ptr1 ptr2
  return (MkBigInt384 fptr2)

foreign import ccall unsafe "bigint384_add" c_bigint384_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt384 -> BigInt384 -> BigInt384
add (MkBigInt384 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint384_add ptr1 ptr2 ptr3
  return (MkBigInt384 fptr3)

foreign import ccall unsafe "bigint384_sub" c_bigint384_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt384 -> BigInt384 -> BigInt384
sub (MkBigInt384 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint384_sub ptr1 ptr2 ptr3
  return (MkBigInt384 fptr3)

foreign import ccall unsafe "bigint384_sqr_truncated" c_bigint384_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt384 -> BigInt384
sqr (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_sqr_truncated ptr1 ptr2
  return (MkBigInt384 fptr2)

foreign import ccall unsafe "bigint384_mul_truncated" c_bigint384_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt384 -> BigInt384 -> BigInt384
mul (MkBigInt384 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint384_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt384 fptr3)

foreign import ccall unsafe "bigint384_shift_left_by_1" c_bigint384_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt384 -> (BigInt384, Bool)
shiftLeft1 (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_shift_left_by_1 ptr1 ptr2
  return (MkBigInt384 fptr2, cret /=0)

foreign import ccall unsafe "bigint384_shift_right_by_1" c_bigint384_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt384 -> (BigInt384, Bool)
shiftRight1 (MkBigInt384 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_shift_right_by_1 ptr1 ptr2
  return (MkBigInt384 fptr2, cret /=0)

foreign import ccall unsafe "bigint384_shift_left_by_k" c_bigint384_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt384 -> Int -> BigInt384
shiftLeft (MkBigInt384 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt384 fptr2)

foreign import ccall unsafe "bigint384_shift_right_by_k" c_bigint384_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt384 -> Int -> BigInt384
shiftRight (MkBigInt384 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint384_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt384 fptr2)
