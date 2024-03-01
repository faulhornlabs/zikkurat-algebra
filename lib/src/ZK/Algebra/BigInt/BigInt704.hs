
-- | 704-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt704
  ( BigInt704(..)
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

to :: Integer -> BigInt704
to = unsafeTo

from :: BigInt704 -> Integer
from = unsafeFrom

zero, one, two :: BigInt704
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt704 where
  (==) = isEqual

instance Num BigInt704 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt704 where
  show = show . from

instance L.Flat BigInt704 where
  sizeInBytes  _pxy = 88
  sizeInQWords _pxy = 11
  withFlat (MkBigInt704 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt704 11

rnd :: IO BigInt704
rnd = do
  fptr <- mallocForeignPtrArray 11
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 11 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt704 fptr)

instance M.Rnd BigInt704 where
  rndIO = rnd

instance C.Ring BigInt704 where
  ringName _ = "BigInt704"
  ringSize _ = 84162174424773976115855838126082058648805436845170781751972494449099714468753293153818664580441415219631727501698851483408310916002940861810045036330430093599283578738055113571066620126149163476340692027772502016
  isZero = ZK.Algebra.BigInt.BigInt704.isZero
  isOne  = ZK.Algebra.BigInt.BigInt704.isOne
  zero   = ZK.Algebra.BigInt.BigInt704.zero
  one    = ZK.Algebra.BigInt.BigInt704.one
  square = ZK.Algebra.BigInt.BigInt704.sqr
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint704_sqr" c_bigint704_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt704 -> BigInt1408
sqrExt (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 22
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_sqr ptr1 ptr2
  return (MkBigInt1408 fptr2)

foreign import ccall unsafe "bigint704_mul" c_bigint704_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt704 -> BigInt704 -> BigInt1408
mulExt (MkBigInt704 fptr1) (MkBigInt704 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 22
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint704_mul ptr1 ptr2 ptr3
  return (MkBigInt1408 fptr3)

foreign import ccall unsafe "bigint704_scale" c_bigint704_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt704 -> BigInt768
scaleExt s (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_scale s ptr1 ptr2
  return (MkBigInt768 fptr2)

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
unsafeMk :: Integer -> IO BigInt704
unsafeMk x = do
  fptr <- mallocForeignPtrArray 11
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 11 x
  return $ MkBigInt704 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt704 -> IO Integer
unsafeGet (MkBigInt704 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 11 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt704
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt704 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint704_is_zero" c_bigint704_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt704 -> Bool
isZero (MkBigInt704 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint704_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint704_is_one" c_bigint704_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt704 -> Bool
isOne (MkBigInt704 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint704_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint704_is_equal" c_bigint704_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt704 -> BigInt704 -> Bool
isEqual (MkBigInt704 fptr1) (MkBigInt704 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint704_set_small" c_bigint704_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt704
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint704_set_small ptr1 x
  return (MkBigInt704 fptr1)

foreign import ccall unsafe "bigint704_neg" c_bigint704_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt704 -> BigInt704
neg (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_neg ptr1 ptr2
  return (MkBigInt704 fptr2)

foreign import ccall unsafe "bigint704_add" c_bigint704_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt704 -> BigInt704 -> BigInt704
add (MkBigInt704 fptr1) (MkBigInt704 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint704_add ptr1 ptr2 ptr3
  return (MkBigInt704 fptr3)

foreign import ccall unsafe "bigint704_sub" c_bigint704_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt704 -> BigInt704 -> BigInt704
sub (MkBigInt704 fptr1) (MkBigInt704 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint704_sub ptr1 ptr2 ptr3
  return (MkBigInt704 fptr3)

foreign import ccall unsafe "bigint704_sqr_truncated" c_bigint704_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt704 -> BigInt704
sqr (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_sqr_truncated ptr1 ptr2
  return (MkBigInt704 fptr2)

foreign import ccall unsafe "bigint704_mul_truncated" c_bigint704_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt704 -> BigInt704 -> BigInt704
mul (MkBigInt704 fptr1) (MkBigInt704 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint704_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt704 fptr3)

foreign import ccall unsafe "bigint704_shift_left_by_1" c_bigint704_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt704 -> (BigInt704, Bool)
shiftLeft1 (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_shift_left_by_1 ptr1 ptr2
  return (MkBigInt704 fptr2, cret /=0)

foreign import ccall unsafe "bigint704_shift_right_by_1" c_bigint704_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt704 -> (BigInt704, Bool)
shiftRight1 (MkBigInt704 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_shift_right_by_1 ptr1 ptr2
  return (MkBigInt704 fptr2, cret /=0)

foreign import ccall unsafe "bigint704_shift_left_by_k" c_bigint704_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt704 -> Int -> BigInt704
shiftLeft (MkBigInt704 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt704 fptr2)

foreign import ccall unsafe "bigint704_shift_right_by_k" c_bigint704_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt704 -> Int -> BigInt704
shiftRight (MkBigInt704 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 11
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint704_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt704 fptr2)
