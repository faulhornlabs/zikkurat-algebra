
-- | 640-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt640
  ( BigInt640(..)
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

to :: Integer -> BigInt640
to = unsafeTo

from :: BigInt640 -> Integer
from = unsafeFrom

zero, one, two :: BigInt640
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt640 where
  (==) = isEqual

instance Num BigInt640 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt640 where
  show = show . from

instance L.Flat BigInt640 where
  sizeInBytes  _pxy = 80
  sizeInQWords _pxy = 10
  withFlat (MkBigInt640 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt640 10

rnd :: IO BigInt640
rnd = do
  fptr <- mallocForeignPtrArray 10
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 10 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt640 fptr)

instance M.Rnd BigInt640 where
  rndIO = rnd

instance C.Ring BigInt640 where
  ringName _ = "BigInt640"
  ringSize _ = 4562440617622195218641171605700291324893228507248559930579192517899275167208677386505912811317371399778642309573594407310688704721375437998252661319722214188251994674360264950082874192246603776
  isZero = ZK.Algebra.BigInt.BigInt640.isZero
  isOne  = ZK.Algebra.BigInt.BigInt640.isOne
  zero   = ZK.Algebra.BigInt.BigInt640.zero
  one    = ZK.Algebra.BigInt.BigInt640.one
  square = ZK.Algebra.BigInt.BigInt640.sqr
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint640_sqr" c_bigint640_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt640 -> BigInt1280
sqrExt (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 20
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_sqr ptr1 ptr2
  return (MkBigInt1280 fptr2)

foreign import ccall unsafe "bigint640_mul" c_bigint640_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt640 -> BigInt640 -> BigInt1280
mulExt (MkBigInt640 fptr1) (MkBigInt640 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 20
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint640_mul ptr1 ptr2 ptr3
  return (MkBigInt1280 fptr3)

foreign import ccall unsafe "bigint640_scale" c_bigint640_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt640 -> BigInt704
scaleExt s (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_scale s ptr1 ptr2
  return (MkBigInt704 fptr2)

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
unsafeMk :: Integer -> IO BigInt640
unsafeMk x = do
  fptr <- mallocForeignPtrArray 10
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 10 x
  return $ MkBigInt640 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt640 -> IO Integer
unsafeGet (MkBigInt640 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 10 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt640
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt640 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint640_is_zero" c_bigint640_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt640 -> Bool
isZero (MkBigInt640 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint640_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint640_is_one" c_bigint640_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt640 -> Bool
isOne (MkBigInt640 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint640_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint640_is_equal" c_bigint640_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt640 -> BigInt640 -> Bool
isEqual (MkBigInt640 fptr1) (MkBigInt640 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint640_set_small" c_bigint640_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt640
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint640_set_small ptr1 x
  return (MkBigInt640 fptr1)

foreign import ccall unsafe "bigint640_neg" c_bigint640_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt640 -> BigInt640
neg (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_neg ptr1 ptr2
  return (MkBigInt640 fptr2)

foreign import ccall unsafe "bigint640_add" c_bigint640_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt640 -> BigInt640 -> BigInt640
add (MkBigInt640 fptr1) (MkBigInt640 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint640_add ptr1 ptr2 ptr3
  return (MkBigInt640 fptr3)

foreign import ccall unsafe "bigint640_sub" c_bigint640_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt640 -> BigInt640 -> BigInt640
sub (MkBigInt640 fptr1) (MkBigInt640 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint640_sub ptr1 ptr2 ptr3
  return (MkBigInt640 fptr3)

foreign import ccall unsafe "bigint640_sqr_truncated" c_bigint640_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt640 -> BigInt640
sqr (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_sqr_truncated ptr1 ptr2
  return (MkBigInt640 fptr2)

foreign import ccall unsafe "bigint640_mul_truncated" c_bigint640_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt640 -> BigInt640 -> BigInt640
mul (MkBigInt640 fptr1) (MkBigInt640 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint640_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt640 fptr3)

foreign import ccall unsafe "bigint640_shift_left_by_1" c_bigint640_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt640 -> (BigInt640, Bool)
shiftLeft1 (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_shift_left_by_1 ptr1 ptr2
  return (MkBigInt640 fptr2, cret /=0)

foreign import ccall unsafe "bigint640_shift_right_by_1" c_bigint640_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt640 -> (BigInt640, Bool)
shiftRight1 (MkBigInt640 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_shift_right_by_1 ptr1 ptr2
  return (MkBigInt640 fptr2, cret /=0)

foreign import ccall unsafe "bigint640_shift_left_by_k" c_bigint640_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt640 -> Int -> BigInt640
shiftLeft (MkBigInt640 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt640 fptr2)

foreign import ccall unsafe "bigint640_shift_right_by_k" c_bigint640_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt640 -> Int -> BigInt640
shiftRight (MkBigInt640 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 10
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint640_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt640 fptr2)
