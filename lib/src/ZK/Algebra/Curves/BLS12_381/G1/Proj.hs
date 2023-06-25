
-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.Curves.BLS12_381.G1.Proj
  ( G1(..)
  , primeP , primeR , cofactor , curveA , curveB
  , genG1 , infinity
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
  , isOnCurve , isInfinity
  , normalize
  , neg , add , dbl , sub
  , sclRef , sclFr
  , rndG1 , rndG1_naive
  )
  where

--------------------------------------------------------------------------------

import Prelude hiding (div)
-- import GHC.Real hiding (div,infinity)

import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.IO.Unsafe

import ZK.Algebra.Curves.BLS12_381.Mont.Fp ( Fp(..) )
import ZK.Algebra.Curves.BLS12_381.Mont.Fr ( Fr(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Mont.Fp as Fp
import qualified ZK.Algebra.Curves.BLS12_381.Mont.Fr as Fr
-- import qualified ZK.Algebra.Class.Curve as C

--------------------------------------------------------------------------------

primeP, primeR, cofactor :: Integer
primeP = Fp.prime
primeR = Fr.prime
cofactor = 76329603384216526031706109802092473003

-- | paramteres A and B of the curve equation @y^2 = x^3 + A*x + B@
curveA, curveB :: Integer
curveA = 0
curveB = 4

-- | generator of the r-sized subgroup G1
genG1 :: G1
genG1 = mkPoint (x, y, Fp.one) where
  x = 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
  y = 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569

--------------------------------------------------------------------------------

-- | An elliptic curve point, in projective coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

-- | Note: this throws an exception if the point is not on the curve
mkPoint :: (Fp, Fp, Fp) -> G1
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Fp, Fp, Fp) -> Maybe G1
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

-- | The point at infinity
infinity :: G1
infinity = unsafeMkPoint (Fp.zero, Fp.one, Fp.zero)

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Fp, Fp, Fp) -> G1
unsafeMkPoint (MkFp fptr1 , MkFp fptr2 , MkFp fptr3) = unsafePerformIO $ do
  fptr4 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr2 $ \ptr3 -> do
        withForeignPtr fptr2 $ \ptr4 -> do
          copyBytes (        ptr4   ) ptr1 48
          copyBytes (plusPtr ptr4 48) ptr2 48
          copyBytes (plusPtr ptr4 96) ptr3 48
  return (MkG1 fptr4)

{-# NOINLINE coords #-}
coords :: G1 -> (Fp, Fp, Fp)
coords (MkG1 fptr4) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 6
  fptr2 <- mallocForeignPtrArray 6
  fptr3 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr2 $ \ptr3 -> do
        withForeignPtr fptr2 $ \ptr4 -> do
          copyBytes ptr1 (        ptr4   ) 48
          copyBytes ptr2 (plusPtr ptr4 48) 48
          copyBytes ptr3 (plusPtr ptr4 96) 48
  return (MkFp fptr1, MkFp fptr2, MkFp fptr3)

-- | Returns a uniformly random element /in the subgroup G1/
-- Note: this is slow because it uses exponentiation
rndG1_naive :: IO G1
rndG1_naive = do
  k <- Fr.rnd :: IO Fr
  return (sclFr k genG1)

-- | Returns a uniformly random element /in the subgroup G1/
rndG1 :: IO G1
rndG1 = rndG1_naive

--------------------------------------------------------------------------------

foreign import ccall unsafe "bls12_381_G1_is_on_curve" c_bls12_381_G1_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G1 -> Bool
isOnCurve (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_is_infinity" c_bls12_381_G1_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G1 -> Bool
isInfinity (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_normalize" c_bls12_381_G1_normalize :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE normalize #-}
normalize :: G1 -> G1
normalize (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_normalize ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_neg" c_bls12_381_G1_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G1 -> G1
neg (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_neg ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_dbl" c_bls12_381_G1_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G1 -> G1
dbl (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_dbl ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_add" c_bls12_381_G1_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: G1 -> G1 -> G1
add (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_add ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_sub" c_bls12_381_G1_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: G1 -> G1 -> G1
sub (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_sub ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_scl_reference" c_bls12_381_G1_scl_reference :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclRef #-}
sclRef :: Fr -> G1 -> G1
sclRef (MkFr fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_scl_reference ptr1 ptr2 ptr3 
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_scl_Fr" c_bls12_381_G1_scl_Fr :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> G1 -> G1
sclFr (MkFr fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_scl_Fr ptr1 ptr2 ptr3 
  return (MkG1 fptr3)
