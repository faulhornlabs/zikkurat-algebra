
-- | Prime field (standard representation) with
--
-- > p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.Curves.BN128.Fr.Std
  ( Fr(..)
  , prime
    -- * Conversion
  , to
  , from
    -- * Field elements
  , small , zero , one , two , primGen
    -- * Predicates
  , isValid , isZero , isOne , isEqual
    -- * Field operations
  , neg , add , sub
  , sqr , mul
  , inv , div , divBy2 , batchInv
    -- * Exponentiation
  , pow , pow_
    -- * FFT
  , fftDomain
    -- * Random
  , rnd
    -- * Export to C
  , exportToCDef , exportListToCDef
  )
  where

--------------------------------------------------------------------------------

import Prelude  hiding (div)
import GHC.Real hiding (div)

import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import ZK.Algebra.BigInt.BigInt256( BigInt256(..) )
import qualified ZK.Algebra.BigInt.BigInt256 as B

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import qualified ZK.Algebra.Class.FFT as T
import           ZK.Algebra.Class.FFT hiding (fftDomain)
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  

newtype Fr = MkFr (ForeignPtr Word64)

prime :: Integer
prime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

to :: Integer -> Fr
to x = unsafeTo (mod x prime)

from :: Fr -> Integer
from = unsafeFrom

zero, one, two :: Fr
zero = small 0
one  = small 1
two  = small 2

primGen :: Fr
primGen = small 5

instance Eq Fr where
  (==) = isEqual

instance Num Fr where
  fromInteger = to
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Fractional Fr where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  (/)   = div

instance Show Fr where
  show = show . from

instance L.Flat Fr where
  sizeInBytes  _pxy = 32
  sizeInQWords _pxy = 4
  withFlat (MkFr fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFr 4

rnd :: IO Fr
rnd = do
  x <- randomRIO (0,prime-1)
  return (unsafeTo x)

instance C.Rnd Fr where
  rndIO = rnd

instance C.Ring Fr where
  ringNamePxy _ = "BN128/Fr (standard repr.)"
  ringSizePxy _ = prime
  isZero = ZK.Algebra.Curves.BN128.Fr.Std.isZero
  isOne  = ZK.Algebra.Curves.BN128.Fr.Std.isOne
  zero   = ZK.Algebra.Curves.BN128.Fr.Std.zero
  one    = ZK.Algebra.Curves.BN128.Fr.Std.one
  square = ZK.Algebra.Curves.BN128.Fr.Std.sqr
  power  = ZK.Algebra.Curves.BN128.Fr.Std.pow
  -- power x e = pow x (B.to (mod e (prime-1)))

instance C.Field Fr where
  charPxy      _ = prime
  dimPxy       _ = 1
  primGenPxy   _ = primGen
  batchInverse   = batchInv
  frobenius      = id
  halve          = divBy2

fftDomain :: FFTSubgroup Fr
fftDomain = MkFFTSubgroup gen 28 where
  gen :: Fr
  gen = to 19103219067921713944291392827692070036145651957329286315305642004821462161904

instance FFTField Fr where
  fftDomain = ZK.Algebra.Curves.BN128.Fr.Std.fftDomain


{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fr -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fr] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 4 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bn128_Fr_std_pow_gen" c_bn128_Fr_std_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

pow :: Fr -> Integer -> Fr
pow x e
  | e == 0      = if isZero x then zero else one
  | e >  0      =      powNonNeg x         e  
  | otherwise   = inv (powNonNeg x (negate e))

withInteger :: Integer -> ((Int, Ptr Word64) -> IO a) -> IO a
withInteger !input action = do
  let (n,ws) = toWord64sLE_ input
  allocaArray n $ \ptr -> do
    pokeArray ptr ws
    action (n,ptr)

{-# NOINLINE powNonNeg #-}
powNonNeg :: Fr -> Integer -> Fr
powNonNeg (MkFr fptr1) expo = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFr fptr3)

{-# NOINLINE pow #-}
powBigInt256 :: Fr -> BigInt256 -> Fr
powBigInt256 (MkFr fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_pow_gen ptr1 ptr2 ptr3 4
  return (MkFr fptr3)

----------------------------------------

foreign import ccall unsafe "bn128_Fr_std_batch_inv" c_bn128_Fr_std_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fr -> FlatArray Fr
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


{-# NOINLINE unsafeMk #-}
unsafeMk :: Integer -> IO Fr
unsafeMk x = do
  fptr <- mallocForeignPtrArray 4
  withForeignPtr fptr $ \ptr -> do
    pokeArray ptr $ toWord64sLE 4 x
  return $ MkFr fptr

{-# NOINLINE unsafeGet #-}
unsafeGet :: Fr -> IO Integer
unsafeGet (MkFr fptr) = do
  ws <- withForeignPtr fptr $ \ptr -> peekArray 4 ptr 
  return (fromWord64sLE ws)

{-# NOINLINE unsafeTo #-}
unsafeTo :: Integer -> Fr
unsafeTo x = unsafePerformIO (unsafeMk x)

{-# NOINLINE unsafeFrom #-}
unsafeFrom :: Fr -> Integer
unsafeFrom f = unsafePerformIO (unsafeGet f)

foreign import ccall unsafe "bn128_Fr_std_is_valid" c_bn128_Fr_std_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fr -> Bool
isValid (MkFr fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fr_std_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_zero" c_bigint256_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fr -> Bool
isZero (MkFr fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_one" c_bigint256_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fr -> Bool
isOne (MkFr fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bigint256_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bigint256_is_equal" c_bigint256_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fr -> Fr -> Bool
isEqual (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bigint256_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bigint256_set_small" c_bigint256_set_small :: Ptr Word64 -> Word64 -> IO ()

{-# NOINLINE small #-}
small :: Word64 -> Fr
small x = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    c_bigint256_set_small ptr1 x
  return (MkFr fptr1)

foreign import ccall unsafe "bn128_Fr_std_neg" c_bn128_Fr_std_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fr -> Fr
neg (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_neg ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bn128_Fr_std_add" c_bn128_Fr_std_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fr -> Fr -> Fr
add (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_add ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bn128_Fr_std_sub" c_bn128_Fr_std_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fr -> Fr -> Fr
sub (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_sub ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bn128_Fr_std_sqr" c_bn128_Fr_std_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fr -> Fr
sqr (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_sqr ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bn128_Fr_std_mul" c_bn128_Fr_std_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fr -> Fr -> Fr
mul (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_mul ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bn128_Fr_std_inv" c_bn128_Fr_std_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fr -> Fr
inv (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_inv ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bn128_Fr_std_div" c_bn128_Fr_std_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fr -> Fr -> Fr
div (MkFr fptr1) (MkFr fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fr_std_div ptr1 ptr2 ptr3
  return (MkFr fptr3)

foreign import ccall unsafe "bn128_Fr_std_div_by_2" c_bn128_Fr_std_div_by_2 :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE divBy2 #-}
divBy2 :: Fr -> Fr
divBy2 (MkFr fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_div_by_2 ptr1 ptr2
  return (MkFr fptr2)

foreign import ccall unsafe "bn128_Fr_std_pow_uint64" c_bn128_Fr_std_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fr -> Word64 -> Fr
pow_ (MkFr fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 4
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fr_std_pow_uint64 ptr1 x ptr2
  return (MkFr fptr2)
