
-- | Univariate polynomials over 'ZK.Algebra.Curves.BLS12_381.Fr.Mont.Fr'
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!
--

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, PatternSynonyms, TypeFamilies, FlexibleInstances #-}
module ZK.Algebra.Curves.BLS12_381.Poly
  ( Poly(..)
    -- * Coefficients
  , coeffs
  , coeffsArr
  , coeffsFlatArr
    -- * Predicates
  , isZero , isEqual
    -- * Queries
  , degree
  , constTermOf
  , kthCoeff
  , evalAt
    -- * Constant polynomials
  , constPoly
  , mbConst
  , zero , one
    -- * Special polynomials
  , idPoly , linearPoly
    -- * Creating polynomials
  , mkPoly , mkPoly' , mkPolyArr , mkPolyFlatArr
    -- * Pretty-printing
  , showPoly, showPoly'
    -- * Ring operations
  , neg , add , sub
  , mul , mulNaive
    -- * Linear combinations
  , scale
    -- * Polynomial division
  , longDiv , quot , rem
  , divByVanishing, quotByVanishing
    -- * Random
  , rndPoly , rnd
  )
  where

--------------------------------------------------------------------------------

import Prelude  hiding (div,quot,rem)
import GHC.Real hiding (div,quot,rem)

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

import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont

import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Poly  as P

import ZK.Algebra.Class.Poly
  ( polyIsOne
  , constTermOf
  , mbConst   
  , constPoly 
  , idPoly    
  , linearPoly
  , showPoly  
  , showPoly' 
  )

--------------------------------------------------------------------------------

newtype Poly = MkPoly (L.FlatArray Fr)

pattern XPoly n arr = MkPoly (L.MkFlatArray n arr)

mkPoly :: [Fr] -> Poly
mkPoly = MkPoly . L.packFlatArrayFromList

mkPoly' :: Int -> [Fr] -> Poly
mkPoly' len xs = MkPoly $ L.packFlatArrayFromList' len xs

mkPolyArr :: Array Int Fr -> Poly
mkPolyArr = MkPoly . L.packFlatArray

mkPolyFlatArr :: L.FlatArray Fr -> Poly
mkPolyFlatArr = MkPoly

coeffs :: Poly -> [Fr]
coeffs (MkPoly arr) = L.unpackFlatArrayToList arr

coeffsArr :: Poly -> Array Int Fr
coeffsArr (MkPoly arr) = L.unpackFlatArray arr

coeffsFlatArr :: Poly -> L.FlatArray Fr
coeffsFlatArr (MkPoly flat) = flat

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
  isZero    = ZK.Algebra.Curves.BLS12_381.Poly.isZero
  isOne     = ZK.Algebra.Curves.BLS12_381.Poly.isOne
  zero      = ZK.Algebra.Curves.BLS12_381.Poly.zero
  one       = ZK.Algebra.Curves.BLS12_381.Poly.one
  power     = error "exponentiation of polynomials is not implemented"

instance L.WrappedArray Poly where
  type Element Poly = Fr
  wrapArray = MkPoly
  unwrapArray (MkPoly flatArr) = flatArr

instance P.Univariate Poly where
  type Coeff Poly = Fr
  degree          = ZK.Algebra.Curves.BLS12_381.Poly.degree
  kthCoeff        = ZK.Algebra.Curves.BLS12_381.Poly.kthCoeff
  evalAt          = ZK.Algebra.Curves.BLS12_381.Poly.evalAt
  scale           = ZK.Algebra.Curves.BLS12_381.Poly.scale
  mkPoly          = ZK.Algebra.Curves.BLS12_381.Poly.mkPoly
  coeffs          = ZK.Algebra.Curves.BLS12_381.Poly.coeffs
  coeffsArr       = ZK.Algebra.Curves.BLS12_381.Poly.coeffsArr
  coeffsFlatArr   = ZK.Algebra.Curves.BLS12_381.Poly.coeffsFlatArr
  polyLongDiv     = ZK.Algebra.Curves.BLS12_381.Poly.longDiv
  polyQuot        = ZK.Algebra.Curves.BLS12_381.Poly.quot
  polyRem         = ZK.Algebra.Curves.BLS12_381.Poly.rem
  divByVanishing  = ZK.Algebra.Curves.BLS12_381.Poly.divByVanishing
  quotByVanishing = ZK.Algebra.Curves.BLS12_381.Poly.quotByVanishing

--------------------------------------------------------------------------------

-- | The constant zero polynomial
zero :: Poly
zero = constPoly 0

-- | The constant one polynomial
one :: Poly
one = constPoly 1

-- | Checks whether the input is the constant one polynomial?
isOne :: Poly -> Bool
isOne p = (mbConst p == Just ZK.Algebra.Curves.BLS12_381.Fr.Mont.one)

-- | @rndPoly d@ generates a random polynomial of degree @d@
rndPoly :: Int -> IO Poly
rndPoly d = mkPoly <$> replicateM (d+1) F.rndIO

-- | @rnd@ generates a random polynomial between degree 0 and 12
rnd :: IO Poly
rnd = do
  d <- randomRIO (0,12)
  rndPoly d

--------------------------------------------------------------------------------


foreign import ccall unsafe "bls12_381_poly_mont_degree"    c_bls12_381_poly_mont_degree    :: CInt -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "bls12_381_poly_mont_get_coeff" c_bls12_381_poly_mont_get_coeff :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_is_zero"   c_bls12_381_poly_mont_is_zero   :: CInt -> Ptr Word64                       -> IO Word8
foreign import ccall unsafe "bls12_381_poly_mont_is_equal"  c_bls12_381_poly_mont_is_equal  :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO Word8
foreign import ccall unsafe "bls12_381_poly_mont_eval_at"   c_bls12_381_poly_mont_eval_at   :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bls12_381_poly_mont_neg"       c_bls12_381_poly_mont_neg       :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_add"       c_bls12_381_poly_mont_add       :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_sub"       c_bls12_381_poly_mont_sub       :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_scale"     c_bls12_381_poly_mont_scale     :: Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_mul_naive" c_bls12_381_poly_mont_mul_naive :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE degree #-}
-- | The degree of a polynomial. By definition, the degree of the constant
-- zero polynomial is @-1@.
degree :: Poly -> Int
degree (XPoly n1 fptr1) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    fromIntegral <$> c_bls12_381_poly_mont_degree (fromIntegral n1) ptr1

{-# NOINLINE kthCoeff #-}
-- | The k-th coefficient of a polynomial.
kthCoeff :: Int -> Poly -> Fr
kthCoeff k (XPoly n1 fptr1) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bls12_381_poly_mont_get_coeff (fromIntegral n1) ptr1 (fromIntegral k) ptr3
      return (MkFr fptr3)

{-# NOINLINE evalAt #-}
-- | Evaluate a polynomial at the given location @x@.
evalAt :: Fr -> Poly -> Fr
evalAt (MkFr fptr2) (XPoly n1 fptr1) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_poly_mont_eval_at (fromIntegral n1) ptr1 ptr2 ptr3
        return (MkFr fptr3)

{-# NOINLINE isZero #-}
-- | Checks whether the given polynomial is the constant zero polynomial
isZero :: Poly -> Bool
isZero (XPoly n1 fptr1) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    c <- c_bls12_381_poly_mont_is_zero (fromIntegral n1) ptr1
    return (c /= 0)

{-# NOINLINE isEqual #-}
-- | Checks whether two polynomials are equal
isEqual :: Poly -> Poly -> Bool
isEqual (XPoly n1 fptr1) (XPoly n2 fptr2) = unsafePerformIO $ do
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c <- c_bls12_381_poly_mont_is_equal (fromIntegral n1) ptr1 (fromIntegral n2) ptr2
      return (c /= 0)

{-# NOINLINE neg #-}
-- | Negate a polynomial
neg :: Poly -> Poly
neg (XPoly n1 fptr1) = unsafePerformIO $ do
  let n3 = n1
  fptr3 <- mallocForeignPtrArray (n3*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bls12_381_poly_mont_neg (fromIntegral n1) ptr1 ptr3
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
        c_bls12_381_poly_mont_add (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
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
        c_bls12_381_poly_mont_sub (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
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
        c_bls12_381_poly_mont_scale ptr1 (fromIntegral n2) ptr2 ptr3
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
        c_bls12_381_poly_mont_mul_naive (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 ptr3
  return (XPoly n3 fptr3)

foreign import ccall unsafe "bls12_381_poly_mont_long_div" c_bls12_381_poly_mont_long_div :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_quot"     c_bls12_381_poly_mont_quot     :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_rem"      c_bls12_381_poly_mont_rem      :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()

{-# NOINLINE longDiv #-}
-- | Polynomial long division
longDiv :: Poly -> Poly -> (Poly, Poly)
longDiv poly1@(XPoly n1 fptr1) poly2@(XPoly n2 fptr2) = unsafePerformIO $ do
  let d2 = degree poly2
  let nq = max 0 (n1-d2)
  let nr = max 0 d2
  fptr3 <- mallocForeignPtrArray (nq*4)
  fptr4 <- mallocForeignPtrArray (nr*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          c_bls12_381_poly_mont_long_div (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 (fromIntegral nq) ptr3 (fromIntegral nr) ptr4
  return (XPoly nq fptr3, XPoly nr fptr4)

{-# NOINLINE quot #-}
-- | Polynomial quotient
quot :: Poly -> Poly -> Poly
quot poly1@(XPoly n1 fptr1) poly2@(XPoly n2 fptr2) = unsafePerformIO $ do
  let d2 = degree poly2
  let nq = max 0 (n1-d2)
  fptr3 <- mallocForeignPtrArray (nq*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_poly_mont_quot (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 (fromIntegral nq) ptr3
  return (XPoly nq fptr3)

{-# NOINLINE rem #-}
-- | Polynomial remainder
rem :: Poly -> Poly -> Poly
rem poly1@(XPoly n1 fptr1) poly2@(XPoly n2 fptr2) = unsafePerformIO $ do
  let d2 = degree poly2
  let nr = max 0 d2
  fptr4 <- mallocForeignPtrArray (nr*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr4 $ \ptr4 -> do
        c_bls12_381_poly_mont_rem (fromIntegral n1) ptr1 (fromIntegral n2) ptr2 (fromIntegral nr) ptr4
  return (XPoly nr fptr4)

foreign import ccall unsafe "bls12_381_poly_mont_div_by_vanishing"  c_bls12_381_poly_mont_div_by_vanishing  :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_poly_mont_quot_by_vanishing" c_bls12_381_poly_mont_quot_by_vanishing :: CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> CInt -> Ptr Word64 -> IO Word8

-- | Divide by the coset vanishing polynomial @(x^n - eta)@.
-- Returns quotient and remainder.
divByVanishing :: Poly -> (Int, Fr) -> (Poly, Poly)
divByVanishing poly1@(XPoly n1 fptr1) (expo_n, MkFr fptr2) = unsafePerformIO $ do
  let d2 = expo_n
  let nq = max 0 (n1-d2)
  let nr = max 0 d2
  fptr3 <- mallocForeignPtrArray (nq*4)
  fptr4 <- mallocForeignPtrArray (nr*4)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          c_bls12_381_poly_mont_div_by_vanishing (fromIntegral n1) ptr1 (fromIntegral expo_n) ptr2 (fromIntegral nq) ptr3 (fromIntegral nr) ptr4
  return (XPoly nq fptr3, XPoly nr fptr4)

-- | Quotient by the coset vanishing polynomial @(x^n - eta)@.
-- Returns @Nothing@ if the remainder is nonzero.
quotByVanishing :: Poly -> (Int, Fr) -> Maybe Poly
quotByVanishing poly1@(XPoly n1 fptr1) (expo_n, MkFr fptr2) = unsafePerformIO $ do
  let d2 = expo_n
  let nq = max 0 (n1-d2)
  fptr3 <- mallocForeignPtrArray (nq*4)
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_poly_mont_quot_by_vanishing (fromIntegral n1) ptr1 (fromIntegral expo_n) ptr2 (fromIntegral nq) ptr3
  return $ if (cret /= 0)
    then Just (XPoly nq fptr3)
    else Nothing
