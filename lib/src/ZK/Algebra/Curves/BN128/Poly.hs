
-- | Univariate polynomials over 'ZK.Algebra.Curves.BN128.Fr.Mont.Fr'
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!
--

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, PatternSynonyms, TypeFamilies, FlexibleInstances #-}
module ZK.Algebra.Curves.BN128.Poly
  ( Poly(..)
    -- * coefficients
  , coeffs , coeffsArr
    -- * predicates
  , isZero , isEqual
    -- * queries
  , degree
  , constTerm
  , kthCoeff
  , evalAt
    -- * constant polynomials
  , constPoly
  , mbConst
  , zero , one
    -- * creating polynomials
  , mkPoly , mkPoly' , mkPolyA
  , linearPoly
    -- * pretty-printing
  , showPoly, showPoly'
    -- * ring operations
  , neg , add , sub
  , mul , mulNaive
    -- * linear combinations
  , scale
    -- * random
  , rndPoly , rnd
  )
  where

--------------------------------------------------------------------------------

import Prelude  hiding (div)
import GHC.Real hiding (div)

import Data.Bits
import Data.Word
import Data.List
import Data.Array

import Control.Monad

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Curves.BN128.Fr.Mont

import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Poly  as P

--------------------------------------------------------------------------------

newtype Poly = MkPoly (L.FlatArray Fr)

pattern XPoly n arr = MkPoly (L.MkFlatArray n arr)

mkPoly :: [Fr] -> Poly
mkPoly = MkPoly . L.packFlatArrayFromList

mkPoly' :: Int -> [Fr] -> Poly
mkPoly' len xs = MkPoly $ L.packFlatArrayFromList' len xs

mkPolyA :: Array Int Fr -> Poly
mkPolyA = MkPoly . L.packFlatArray

coeffs :: Poly -> [Fr]
coeffs (MkPoly arr) = L.unpackFlatArrayToList arr

coeffsArr :: Poly -> Array Int Fr
coeffsArr (MkPoly arr) = L.unpackFlatArray arr

--------------------------------------------------------------------------------

instance Eq Poly where
  (==) = isEqual

instance Show Poly where
  show = showPoly' False

instance Num Poly where
  fromInteger = constPoly . fromInteger
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> constPoly 1

mul = mulNaive       -- TEMPORARY !!!

instance F.Rnd Poly where
  rndIO = rnd

instance F.Ring Poly where
  ringNamePxy _ = "Fr[x]"
  ringSizePxy _ = error "this is a polynomial ring, it's not finite"
  isZero    = ZK.Algebra.Curves.BN128.Poly.isZero
  isOne     = ZK.Algebra.Curves.BN128.Poly.isOne
  zero      = ZK.Algebra.Curves.BN128.Poly.zero
  one       = ZK.Algebra.Curves.BN128.Poly.one
  power     = error "exponentiation of polynomials is not implemented"

instance P.Univariate Poly where
  type Coeff Poly = Fr
  degree    = ZK.Algebra.Curves.BN128.Poly.degree
  coeffs    = ZK.Algebra.Curves.BN128.Poly.coeffs
  coeffsArr = ZK.Algebra.Curves.BN128.Poly.coeffsArr
  kthCoeff  = ZK.Algebra.Curves.BN128.Poly.kthCoeff
  mkPoly    = ZK.Algebra.Curves.BN128.Poly.mkPoly
  evalAt    = ZK.Algebra.Curves.BN128.Poly.evalAt
  scale    = ZK.Algebra.Curves.BN128.Poly.scale

--------------------------------------------------------------------------------

-- | The constant zero polynomial
zero :: Poly
zero = constPoly 0

-- | The constant one polynomial
one :: Poly
one = constPoly 1

-- | Checks whether the input is the constant one polynomial?
isOne :: Poly -> Bool
isOne p = (mbConst p == Just ZK.Algebra.Curves.BN128.Fr.Mont.one)

-- | The constant term of a polynomial
constTerm :: Poly -> Fr
constTerm p = kthCoeff 0 p

-- | Is this a constant polynomial?
mbConst :: Poly -> Maybe Fr
mbConst p = if degree p <= 0 then Just (constTerm p) else Nothing

-- | Create a constant polynomial
constPoly :: Fr -> Poly
constPoly y = mkPoly [y]

-- | @linearPoly a b == a*x + b@
linearPoly :: Fr -> Fr -> Poly
linearPoly a b = mkPoly [b,a]

showPoly :: Poly -> String
showPoly = showPoly' True

showPoly' :: Bool -> Poly -> String
showPoly' newlines_flag poly =
  case newlines_flag of
    False -> intercalate " +"   terms
    True  -> intercalate " +\n" terms
  where
    terms = zipWith f [0..] (coeffs poly)
    f k x = ' ' : show x ++ " * x^" ++ show k

-- | @rndPoly d@ generates a random polynomial of degree @d@
rndPoly :: Int -> IO Poly
rndPoly d = mkPoly <$> replicateM (d+1) F.rndIO

-- | @rnd@ generates a random polynomial between degree 0 and 12
rnd :: IO Poly
rnd = do
  d <- randomRIO (0,12)
  rndPoly d

--------------------------------------------------------------------------------


foreign import ccall unsafe "bn128_poly_mont_degree"    c_bn128_poly_mont_degree    :: CInt -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "bn128_poly_mont_get_coeff" c_bn128_poly_mont_get_coeff :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_poly_mont_is_zero"   c_bn128_poly_mont_is_zero   :: CInt -> Ptr Word64                       -> IO Word8
foreign import ccall unsafe "bn128_poly_mont_is_equal"  c_bn128_poly_mont_is_equal  :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO Word8
foreign import ccall unsafe "bn128_poly_mont_eval_at"   c_bn128_poly_mont_eval_at   :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bn128_poly_mont_neg"       c_bn128_poly_mont_neg       :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_poly_mont_add"       c_bn128_poly_mont_add       :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_poly_mont_sub"       c_bn128_poly_mont_sub       :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_poly_mont_scale"     c_bn128_poly_mont_scale     :: Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_poly_mont_mul_naive" c_bn128_poly_mont_mul_naive :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE degree #-}
-- | The degree of a polynomial. By definition, the degree of the constant
-- zero polynomial is @-1@.
degree :: Poly -> Int
degree (XPoly n1 fptr1) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    fromIntegral <$> c_bn128_poly_mont_degree (fromIntegral n1) ptr1

{-# NOINLINE kthCoeff #-}
-- | The k-th coefficient of a polynomial.
kthCoeff :: Int -> Poly -> Fr
kthCoeff k (XPoly n1 fptr1) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bn128_poly_mont_get_coeff (fromIntegral n1) ptr1 (fromIntegral k) ptr3
      return (MkFr fptr3)

{-# NOINLINE evalAt #-}
-- | Evaluate a polynomial at the given location @x@.
evalAt :: Fr -> Poly -> Fr
evalAt (MkFr fptr2) (XPoly n1 fptr1) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_poly_mont_eval_at (fromIntegral n1) ptr1 ptr2 ptr3
        return (MkFr fptr3)

{-# NOINLINE isZero #-}
-- | Checks whether the given polynomial is the constant zero polynomial
isZero :: Poly -> Bool
isZero (XPoly n1 fptr1) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    c <- c_bn128_poly_mont_is_zero (fromIntegral n1) ptr1
    return (c /= 0)

{-# NOINLINE isEqual #-}
-- | Checks whether two polynomials are equal
isEqual :: Poly -> Poly -> Bool
isEqual (XPoly n1 fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c <- c_bn128_poly_mont_is_equal (fromIntegral n1) ptr1 (fromIntegral n2) ptr2
      return (c /= 0)

{-# NOINLINE neg #-}
-- | Negate a polynomial
neg :: Poly -> Poly
neg (XPoly n1 fptr1) = unsafePerformIO $ do
  let n3 = n1
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bn128_poly_mont_neg (fromIntegral n1) ptr1 ptr3
  return (XPoly n3 fptr3)

{-# NOINLINE add #-}
-- | Adds two polynomials
add :: Poly -> Poly -> Poly
add (XPoly n1 fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  let n3 = max n1 n2
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_poly_mont_add (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
  return (XPoly n3 fptr3)

{-# NOINLINE sub #-}
-- | Subtracts two polynomials
sub :: Poly -> Poly -> Poly
sub (XPoly n1 fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  let n3 = max n1 n2
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_poly_mont_sub (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
  return (XPoly n3 fptr3)

{-# NOINLINE scale #-}
-- | Multiplies a polynomial by a constant
scale :: Fr -> Poly -> Poly
scale (MkFr fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  let n3 = n2
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_poly_mont_scale ptr1 (fromIntegral n2) ptr2 ptr3
  return (XPoly n3 fptr3)

{-# NOINLINE mulNaive #-}
-- | Multiplication of polynomials, naive algorithm
mulNaive :: Poly -> Poly -> Poly
mulNaive (XPoly n1 fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  let n3 = n1 + n2 - 1
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_poly_mont_mul_naive (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
  return (XPoly n3 fptr3)
