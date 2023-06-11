
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt448
  ( BigInt448(..)
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

to :: Integer -> BigInt448
to = unsafeTo

from :: BigInt448 -> Integer
from = unsafeFrom

zero, one, two :: BigInt448
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt448 where
  (==) = isEqual

instance Num BigInt448 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt448 where
  show = show . from

rnd :: IO BigInt448
rnd = do
  fptr <- mallocForeignPtrArray 7
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 7 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt448 fptr)

instance C.Rnd BigInt448 where
  rndIO = rnd

instance C.Ring BigInt448 where
  ringNamePxy _ = "BigInt448"
  ringSizePxy _ = 726838724295606890549323807888004534353641360687318060281490199180639288113397923326191050713763565560762521606266177933534601628614656
  isZero = isZero
  isOne  = isOne
  zero   = zero
  one    = one
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint448_sqr" c_bigint448_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt448 -> BigInt896
sqrExt (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 14
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_sqr ptr1 ptr2
  return (MkBigInt896 fptr2)

foreign import ccall unsafe "bigint448_mul" c_bigint448_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt448 -> BigInt448 -> BigInt896
mulExt (MkBigInt448 fptr1) (MkBigInt448 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 14
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint448_mul ptr1 ptr2 ptr3
  return (MkBigInt896 fptr3)

foreign import ccall unsafe "bigint448_scale" c_bigint448_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt448 -> BigInt512
scaleExt s (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_scale s ptr1 ptr2
  return (MkBigInt512 fptr2)

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
unsafeMk :: Integer -> IO BigInt448
unsafeMk x = do
  fptr <- mallocForeignPtrArray 7
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE' 7 x
  return $ MkBigInt448 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt448 -> IO Integer
unsafeGet (MkBigInt448 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 7 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt448
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt448 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint448_is_zero" c_bigint448_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt448 -> Bool
isZero (MkBigInt448 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint448_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint448_is_one" c_bigint448_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt448 -> Bool
isOne (MkBigInt448 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint448_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint448_is_equal" c_bigint448_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt448 -> BigInt448 -> Bool
isEqual (MkBigInt448 fptr1) (MkBigInt448 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint448_set_small" c_bigint448_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt448
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint448_set_small ptr1 x
  return (MkBigInt448 fptr1)

foreign import ccall unsafe "bigint448_neg" c_bigint448_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt448 -> BigInt448
neg (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_neg ptr1 ptr2
  return (MkBigInt448 fptr2)

foreign import ccall unsafe "bigint448_add" c_bigint448_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt448 -> BigInt448 -> BigInt448
add (MkBigInt448 fptr1) (MkBigInt448 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint448_add ptr1 ptr2 ptr3
  return (MkBigInt448 fptr3)

foreign import ccall unsafe "bigint448_sub" c_bigint448_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt448 -> BigInt448 -> BigInt448
sub (MkBigInt448 fptr1) (MkBigInt448 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint448_sub ptr1 ptr2 ptr3
  return (MkBigInt448 fptr3)

foreign import ccall unsafe "bigint448_sqr_truncated" c_bigint448_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt448 -> BigInt448
sqr (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_sqr_truncated ptr1 ptr2
  return (MkBigInt448 fptr2)

foreign import ccall unsafe "bigint448_mul_truncated" c_bigint448_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt448 -> BigInt448 -> BigInt448
mul (MkBigInt448 fptr1) (MkBigInt448 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint448_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt448 fptr3)

foreign import ccall unsafe "bigint448_shift_left_by_1" c_bigint448_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt448 -> (BigInt448, Bool)
shiftLeft1 (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_shift_left_by_1 ptr1 ptr2
  return (MkBigInt448 fptr2, cret /=0)

foreign import ccall unsafe "bigint448_shift_right_by_1" c_bigint448_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt448 -> (BigInt448, Bool)
shiftRight1 (MkBigInt448 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_shift_right_by_1 ptr1 ptr2
  return (MkBigInt448 fptr2, cret /=0)

foreign import ccall unsafe "bigint448_shift_left_by_k" c_bigint448_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt448 -> Int -> BigInt448
shiftLeft (MkBigInt448 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt448 fptr2)

foreign import ccall unsafe "bigint448_shift_right_by_k" c_bigint448_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt448 -> Int -> BigInt448
shiftRight (MkBigInt448 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 7
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint448_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt448 fptr2)
