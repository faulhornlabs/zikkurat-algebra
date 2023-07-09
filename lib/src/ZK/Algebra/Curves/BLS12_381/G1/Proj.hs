
-- | BLS12-381 curve, projective coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies #-}
module ZK.Algebra.Curves.BLS12_381.G1.Proj
  ( G1(..)
  , primeP , primeR , cofactor , curveA , curveB
  , genG1 , infinity
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
  , fromAffine , toAffine
  , isEqual , isSame
  , isOnCurve , isInfinity , isInSubgroup
  , normalize
  , neg , add , madd, dbl , sub
  , sclFr , sclBig , sclSmall
  , rndG1 , rndG1_naive
  , msm , msmStd
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

import ZK.Algebra.Curves.BLS12_381.Fp.Mont ( Fp(..) )
import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont as Fp
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt384 as BigP

import {-# SOURCE #-} qualified ZK.Algebra.Curves.BLS12_381.G1.Affine

import           ZK.Algebra.Class.Flat ( FlatArray(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C

--------------------------------------------------------------------------------

-- | The sizes of the fields Fp, Fr, and the cofactor of the subgroup G1
primeP, primeR, cofactor :: Integer
primeP = Fp.prime
primeR = Fr.prime
cofactor = 76329603384216526031706109802092473003

-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@
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
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
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
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          copyBytes ptr1 (        ptr4   ) 48
          copyBytes ptr2 (plusPtr ptr4 48) 48
          copyBytes ptr3 (plusPtr ptr4 96) 48
  return (MkFp fptr1, MkFp fptr2, MkFp fptr3)

-- | Returns a uniformly random element /in the subgroup G1/.
-- Note: this is slow, because it uses exponentiation.
rndG1_naive :: IO G1
rndG1_naive = do
  k <- Fr.rnd :: IO Fr
  return (sclFr k genG1)

-- | Returns a uniformly random element /in the subgroup G1/
rndG1 :: IO G1
rndG1 = rndG1_naive

--------------------------------------------------------------------------------

instance C.StrictEq G1 where
  (===) = isSame

instance Eq G1 where
  (==) = isEqual
  -- p == q  =  coords (normalize p) == coords (normalize q)

instance Show G1 where
  show pt = case coords pt of
     (x,y,z) -> "[ " ++ show x ++ " : " ++ show y ++ " : " ++ show z ++ " ]"

instance L.Flat G1 where
  sizeInBytes  _pxy = 144
  sizeInQWords _pxy = 18

instance F.Rnd G1 where
  rndIO = rndG1

instance C.Group G1 where
  grpName _    = "BLS12-381 / G1"
  grpIsUnit    = ZK.Algebra.Curves.BLS12_381.G1.Proj.isInfinity
  grpUnit      = ZK.Algebra.Curves.BLS12_381.G1.Proj.infinity
  grpNormalize = normalize
  grpNeg       = neg
  grpDbl       = dbl
  grpAdd       = add
  grpSub       = sub
  grpScale_    = sclSmall
  grpScale     = sclBig

instance C.Curve G1 where
  curveNamePxy _ = "BLS12-381 ( Fp )"
  type BaseField   G1 = Fp
  type ScalarField G1 = Fr
  isOnCurve   = ZK.Algebra.Curves.BLS12_381.G1.Proj.isOnCurve
  isInifinity = ZK.Algebra.Curves.BLS12_381.G1.Proj.isInfinity
  infinity    = ZK.Algebra.Curves.BLS12_381.G1.Proj.infinity
  subgroupGen = ZK.Algebra.Curves.BLS12_381.G1.Proj.genG1
  scalarMul   = ZK.Algebra.Curves.BLS12_381.G1.Proj.sclFr

instance C.ProjCurve G1 where
  type AffinePoint G1 = ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
  fromAffine = ZK.Algebra.Curves.BLS12_381.G1.Proj.fromAffine
  toAffine   = ZK.Algebra.Curves.BLS12_381.G1.Proj.toAffine
  coords3    = ZK.Algebra.Curves.BLS12_381.G1.Proj.coords
  mkPoint3   = ZK.Algebra.Curves.BLS12_381.G1.Proj.mkPoint
  mixedAdd   = ZK.Algebra.Curves.BLS12_381.G1.Proj.madd
  
--------------------------------------------------------------------------------

sclSmall :: Int -> G1 -> G1
sclSmall k pt
  | k == 0    = infinity
  | k < 0     = neg $ sclSmallNonNeg (negate k) pt
  | otherwise =       sclSmallNonNeg (       k) pt

sclBig :: Integer -> G1 -> G1
sclBig k pt
  | k == 0    = infinity
  | k < 0     = neg $ sclBigNonNeg (fromInteger $ negate k) pt
  | otherwise =       sclBigNonNeg (fromInteger $        k) pt

--------------------------------------------------------------------------------


foreign import ccall unsafe "bls12_381_G1_proj_MSM_std_coeff_proj_out" c_bls12_381_G1_proj_MSM_std_coeff_proj_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()
foreign import ccall unsafe "bls12_381_G1_proj_MSM_mont_coeff_proj_out" c_bls12_381_G1_proj_MSM_mont_coeff_proj_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE msm #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in Montgomery representation,
-- and the curve points in affine coordinates
-- 
-- > msmStd :: FlatArray Fr -> FlatArray Affine.G1 -> G1
-- 
msm :: FlatArray Fr -> FlatArray ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> G1
msm (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msm: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 18
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bls12_381_G1_proj_MSM_mont_coeff_proj_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG1 fptr3)

{-# NOINLINE msmStd #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in standard representation,
-- and the curve points in affine coordinates
-- 
-- > msmStd :: FlatArray Std.Fr -> FlatArray Affine.G1 -> G1
-- 
msmStd :: FlatArray ZK.Algebra.Curves.BLS12_381.Fr.Std.Fr -> FlatArray ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> G1
msmStd (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msm: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 18
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bls12_381_G1_proj_MSM_mont_coeff_proj_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG1 fptr3)


foreign import ccall unsafe "bls12_381_G1_proj_is_on_curve" c_bls12_381_G1_proj_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G1 -> Bool
isOnCurve (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_proj_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_proj_is_infinity" c_bls12_381_G1_proj_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G1 -> Bool
isInfinity (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_proj_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_proj_is_in_subgroup" c_bls12_381_G1_proj_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G1 -> Bool
isInSubgroup (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_proj_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_proj_is_equal" c_bls12_381_G1_proj_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G1 -> G1 -> Bool
isEqual (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_proj_is_same" c_bls12_381_G1_proj_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G1 -> G1 -> Bool
isSame (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_proj_normalize" c_bls12_381_G1_proj_normalize :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE normalize #-}
normalize :: G1 -> G1
normalize (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_normalize ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_proj_from_affine" c_bls12_381_G1_proj_from_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE fromAffine #-}
fromAffine :: ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Proj.G1
fromAffine (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_from_affine ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_proj_to_affine" c_bls12_381_G1_proj_to_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE toAffine #-}
toAffine :: ZK.Algebra.Curves.BLS12_381.G1.Proj.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
toAffine (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_to_affine ptr1 ptr2
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_proj_neg" c_bls12_381_G1_proj_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G1 -> G1
neg (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_neg ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_proj_dbl" c_bls12_381_G1_proj_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G1 -> G1
dbl (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_proj_dbl ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_proj_add" c_bls12_381_G1_proj_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: G1 -> G1 -> G1
add (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_add ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_proj_sub" c_bls12_381_G1_proj_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: G1 -> G1 -> G1
sub (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_sub ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_proj_madd_proj_aff" c_bls12_381_G1_proj_madd_proj_aff :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE madd #-}
madd :: G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> G1
madd (MkG1 fptr1) (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_madd_proj_aff ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_proj_scl_Fr" c_bls12_381_G1_proj_scl_Fr :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> G1 -> G1
sclFr (MkFr fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_scl_Fr ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_proj_scl_big" c_bls12_381_G1_proj_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt384 -> G1 -> G1
sclBigNonNeg (BigP.MkBigInt384 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_scl_big ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_proj_scl_small" c_bls12_381_G1_proj_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> G1 -> G1
sclSmallNonNeg k1 (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bls12_381_G1_proj_scl_small (fromIntegral k1) ptr2 ptr3
  return (MkG1 fptr3)
