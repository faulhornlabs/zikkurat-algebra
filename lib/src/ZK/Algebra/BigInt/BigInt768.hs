
-- | 768-bit unsigned integers
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.BigInt768
  ( BigInt768(..)
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

to :: Integer -> BigInt768
to = unsafeTo

from :: BigInt768 -> Integer
from = unsafeFrom

zero, one, two :: BigInt768
zero = small 0
one  = small 1
two  = small 2

instance Eq BigInt768 where
  (==) = isEqual

instance Num BigInt768 where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Show BigInt768 where
  show = show . from

instance L.Flat BigInt768 where
  sizeInBytes  _pxy = 96
  sizeInQWords _pxy = 12
  withFlat (MkBigInt768 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkBigInt768 12

rnd :: IO BigInt768
rnd = do
  fptr <- mallocForeignPtrArray 12
  withForeignPtr fptr $ \ptr -> do
    xs <- replicateM 12 (randomIO :: IO Word64)
    pokeArray ptr xs
  return (MkBigInt768 fptr)

instance M.Rnd BigInt768 where
  rndIO = rnd

instance C.Ring BigInt768 where
  ringName _ = "BigInt768"
  ringSize _ = 1552518092300708935148979488462502555256886017116696611139052038026050952686376886330878408828646477950487730697131073206171580044114814391444287275041181139204454976020849905550265285631598444825262999193716468750892846853816057856
  isZero = ZK.Algebra.BigInt.BigInt768.isZero
  isOne  = ZK.Algebra.BigInt.BigInt768.isOne
  zero   = ZK.Algebra.BigInt.BigInt768.zero
  one    = ZK.Algebra.BigInt.BigInt768.one
  square = ZK.Algebra.BigInt.BigInt768.sqr
  power  = C.ringPowerDefault

----------------------------------------

foreign import ccall unsafe "bigint768_sqr" c_bigint768_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqrExt #-}
sqrExt :: BigInt768 -> BigInt1536
sqrExt (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_sqr ptr1 ptr2
  return (MkBigInt1536 fptr2)

foreign import ccall unsafe "bigint768_mul" c_bigint768_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mulExt #-}
mulExt :: BigInt768 -> BigInt768 -> BigInt1536
mulExt (MkBigInt768 fptr1) (MkBigInt768 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint768_mul ptr1 ptr2 ptr3
  return (MkBigInt1536 fptr3)

foreign import ccall unsafe "bigint768_scale" c_bigint768_scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE scaleExt #-}
scaleExt :: Word64 -> BigInt768 -> BigInt832
scaleExt s (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 13
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_scale s ptr1 ptr2
  return (MkBigInt832 fptr2)

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
unsafeMk :: Integer -> IO BigInt768
unsafeMk x = do
  fptr <- mallocForeignPtrArray 12
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 12 x
  return $ MkBigInt768 fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: BigInt768 -> IO Integer
unsafeGet (MkBigInt768 fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 12 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> BigInt768
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: BigInt768 -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bigint768_is_zero" c_bigint768_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: BigInt768 -> Bool
isZero (MkBigInt768 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint768_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint768_is_one" c_bigint768_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: BigInt768 -> Bool
isOne (MkBigInt768 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint768_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint768_is_equal" c_bigint768_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: BigInt768 -> BigInt768 -> Bool
isEqual (MkBigInt768 fptr1) (MkBigInt768 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint768_set_small" c_bigint768_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> BigInt768
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint768_set_small ptr1 x
  return (MkBigInt768 fptr1)

foreign import ccall unsafe "bigint768_neg" c_bigint768_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: BigInt768 -> BigInt768
neg (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_neg ptr1 ptr2
  return (MkBigInt768 fptr2)

foreign import ccall unsafe "bigint768_add" c_bigint768_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: BigInt768 -> BigInt768 -> BigInt768
add (MkBigInt768 fptr1) (MkBigInt768 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint768_add ptr1 ptr2 ptr3
  return (MkBigInt768 fptr3)

foreign import ccall unsafe "bigint768_sub" c_bigint768_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: BigInt768 -> BigInt768 -> BigInt768
sub (MkBigInt768 fptr1) (MkBigInt768 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint768_sub ptr1 ptr2 ptr3
  return (MkBigInt768 fptr3)

foreign import ccall unsafe "bigint768_sqr_truncated" c_bigint768_sqr_truncated :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: BigInt768 -> BigInt768
sqr (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_sqr_truncated ptr1 ptr2
  return (MkBigInt768 fptr2)

foreign import ccall unsafe "bigint768_mul_truncated" c_bigint768_mul_truncated :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: BigInt768 -> BigInt768 -> BigInt768
mul (MkBigInt768 fptr1) (MkBigInt768 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bigint768_mul_truncated ptr1 ptr2 ptr3
  return (MkBigInt768 fptr3)

foreign import ccall unsafe "bigint768_shift_left_by_1" c_bigint768_shift_left_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftLeft1 #-}
shiftLeft1 :: BigInt768 -> (BigInt768, Bool)
shiftLeft1 (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_shift_left_by_1 ptr1 ptr2
  return (MkBigInt768 fptr2, cret /=0)

foreign import ccall unsafe "bigint768_shift_right_by_1" c_bigint768_shift_right_by_1 :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE shiftRight1 #-}
shiftRight1 :: BigInt768 -> (BigInt768, Bool)
shiftRight1 (MkBigInt768 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_shift_right_by_1 ptr1 ptr2
  return (MkBigInt768 fptr2, cret /=0)

foreign import ccall unsafe "bigint768_shift_left_by_k" c_bigint768_shift_left_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftLeft #-}
shiftLeft :: BigInt768 -> Int -> BigInt768
shiftLeft (MkBigInt768 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_shift_left_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt768 fptr2)

foreign import ccall unsafe "bigint768_shift_right_by_k" c_bigint768_shift_right_by_k :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE shiftRight #-}
shiftRight :: BigInt768 -> Int -> BigInt768
shiftRight (MkBigInt768 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint768_shift_right_by_k ptr1 ptr2 (fromIntegral x)
  return (MkBigInt768 fptr2)
