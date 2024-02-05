
-- | 576-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt576
  ( BigInt576(..)
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

to :: Integer -> BigInt576
to = unsafeTo

from :: BigInt576 -> Integer
from = unsafeFrom

zero, one, two :: BigInt576
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt576 where
  (==) = isEqual

instance Num BigInt576 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt576 where
  show = show . from

instance L.Flat BigInt576 where
  sizeInBytes  _pxy = 72
  sizeInQWords _pxy = 9
  withFlat (MkBigInt576 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt576 9

rnd :: IO BigInt576
rnd = do
  fptr <- mallocForeignPtrArray 9
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 9 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt576 fptr)

instance M.Rnd BigInt576 where
  rndIO = rnd

instance C.Ring BigInt576 where
  ringNamePxy _ = "BigInt576"
  ringSizePxy _ = 247330401473104534060502521019647190035131349101211839914063056092897225106531867170316401061243044989597671426016139339351365034306751209967546155101893167916606772148699136
  isZero = ZK.Algebra.BigInt.BigInt576.isZero
  isOne  = ZK.Algebra.BigInt.BigInt576.isOne
  zero   = ZK.Algebra.BigInt.BigInt576.zero
  one    = ZK.Algebra.BigInt.BigInt576.one
  square = ZK.Algebra.BigInt.BigInt576.sqr
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint576_sqr" c_bigint576_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt576 -> BigInt1152
sqrExt (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_sqr ptr1 ptr2
  return (MkBigInt1152 fptr2)

foreign import ccall unsafe "bigint576_mul" c_bigint576_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt576 -> BigInt576 -> BigInt1152
mulExt (MkBigInt576 fptr1) (MkBigInt576 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint576_mul ptr1 ptr2 ptr3
  return (MkBigInt1152 fptr3)

foreign import ccall unsafe "bigint576_scale" c_bigint576_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt576 -> BigInt640
scaleExt s (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_scale s ptr1 ptr2
  return (MkBigInt640 fptr2)

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
unsafeMk :: Integer -> IO BigInt576
unsafeMk x = do
  fptr <- mallocForeignPtrArray 9
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 9 x
  return $ MkBigInt576 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt576 -> IO Integer
unsafeGet (MkBigInt576 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 9 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt576
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt576 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint576_is_zero" c_bigint576_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt576 -> Bool
isZero (MkBigInt576 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint576_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint576_is_one" c_bigint576_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt576 -> Bool
isOne (MkBigInt576 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint576_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint576_is_equal" c_bigint576_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt576 -> BigInt576 -> Bool
isEqual (MkBigInt576 fptr1) (MkBigInt576 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint576_set_small" c_bigint576_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt576
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint576_set_small ptr1 x
  return (MkBigInt576 fptr1)

foreign import ccall unsafe "bigint576_neg" c_bigint576_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt576 -> BigInt576
neg (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_neg ptr1 ptr2
  return (MkBigInt576 fptr2)

foreign import ccall unsafe "bigint576_add" c_bigint576_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt576 -> BigInt576 -> BigInt576
add (MkBigInt576 fptr1) (MkBigInt576 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint576_add ptr1 ptr2 ptr3
  return (MkBigInt576 fptr3)

foreign import ccall unsafe "bigint576_sub" c_bigint576_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt576 -> BigInt576 -> BigInt576
sub (MkBigInt576 fptr1) (MkBigInt576 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint576_sub ptr1 ptr2 ptr3
  return (MkBigInt576 fptr3)

foreign import ccall unsafe "bigint576_sqr_truncated" c_bigint576_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt576 -> BigInt576
sqr (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_sqr_truncated ptr1 ptr2
  return (MkBigInt576 fptr2)

foreign import ccall unsafe "bigint576_mul_truncated" c_bigint576_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt576 -> BigInt576 -> BigInt576
mul (MkBigInt576 fptr1) (MkBigInt576 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint576_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt576 fptr3)

foreign import ccall unsafe "bigint576_shift_left_by_1" c_bigint576_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt576 -> (BigInt576, Bool)
shiftLeft1 (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_shift_left_by_1 ptr1 ptr2
  return (MkBigInt576 fptr2, cret /=0)

foreign import ccall unsafe "bigint576_shift_right_by_1" c_bigint576_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt576 -> (BigInt576, Bool)
shiftRight1 (MkBigInt576 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_shift_right_by_1 ptr1 ptr2
  return (MkBigInt576 fptr2, cret /=0)

foreign import ccall unsafe "bigint576_shift_left_by_k" c_bigint576_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt576 -> Int -> BigInt576
shiftLeft (MkBigInt576 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt576 fptr2)

foreign import ccall unsafe "bigint576_shift_right_by_k" c_bigint576_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt576 -> Int -> BigInt576
shiftRight (MkBigInt576 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 9
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint576_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt576 fptr2)
