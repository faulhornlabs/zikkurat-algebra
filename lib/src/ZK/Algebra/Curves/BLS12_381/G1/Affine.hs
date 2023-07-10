
-- | BLS12-381 curve, affine coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies #-}
module ZK.Algebra.Curves.BLS12_381.G1.Affine
  ( G1(..)
  , primeP , primeR , cofactor , curveA , curveB
  , genG1 , infinity
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
  , isEqual , isSame
  , isInfinity , isOnCurve , isInSubgroup
  , neg , add , dbl , sub
  , sclFr , sclBig , sclSmall
  , rndG1
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
import qualified ZK.Algebra.BigInt.BigInt384 as BigP
import qualified ZK.Algebra.Curves.BLS12_381.G1.Proj as Proj    -- note: be careful with cyclic imports!

import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C

--------------------------------------------------------------------------------

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
genG1 = mkPoint (x, y) where
  x = 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
  y = 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569

--------------------------------------------------------------------------------

-- | An elliptic curve point, in affine coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

-- | Note: this throws an exception if the point is not on the curve
mkPoint :: (Fp, Fp) -> G1
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Fp, Fp) -> Maybe G1
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Fp, Fp) -> G1
unsafeMkPoint (MkFp fptr1 , MkFp fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes (        ptr3   ) ptr1 48
        copyBytes (plusPtr ptr3 48) ptr2 48
  return (MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_affine_set_infinity" c_bls12_381_G1_affine_set_infinity :: Ptr Word64 -> IO ()

-- | The point at infinity (represented as the special string @0xffff...ffff@)
{-# NOINLINE infinity #-}
infinity :: G1
infinity = unsafePerformIO $ do
  fptr <- mallocForeignPtrArray 12
  withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_affine_set_infinity ptr
  return (MkG1 fptr)

{-# NOINLINE coords #-}
-- | Affine coordinates (TODO: handle the point at infinity)
coords :: G1 -> (Fp, Fp)
coords (MkG1 fptr3) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 6
  fptr2 <- mallocForeignPtrArray 6
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes ptr1 (        ptr3   ) 48
        copyBytes ptr2 (plusPtr ptr3 48) 48
  return (MkFp fptr1, MkFp fptr2)

-- | Returns a uniformly random element /in the subgroup G1/
rndG1 :: IO G1
rndG1 = Proj.toAffine <$> Proj.rndG1

--------------------------------------------------------------------------------

instance C.StrictEq G1 where
  (===) = isSame

instance Eq G1 where
  (==) = isEqual

instance Show G1 where
  show pt
    | isInfinity pt = "<point-at-infinity>"
    | otherwise = case coords pt of
        (x,y) -> "( " ++ show x ++ " , " ++ show y ++ " )"

instance L.Flat G1 where
  sizeInBytes  _pxy = 96
  sizeInQWords _pxy = 12
  withFlat (MkG1 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG1 12

instance F.Rnd G1 where
  rndIO = rndG1

instance C.Group G1 where
  grpName _    = "BLS12-381 / G1"
  grpIsUnit    = isInfinity
  grpUnit      = infinity
  grpNormalize = id
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
  isOnCurve   = ZK.Algebra.Curves.BLS12_381.G1.Affine.isOnCurve
  isInifinity = ZK.Algebra.Curves.BLS12_381.G1.Affine.isInfinity
  infinity    = ZK.Algebra.Curves.BLS12_381.G1.Affine.infinity
  subgroupGen = ZK.Algebra.Curves.BLS12_381.G1.Affine.genG1
  scalarMul   = ZK.Algebra.Curves.BLS12_381.G1.Affine.sclFr

instance C.AffineCurve G1 where
  coords2    = ZK.Algebra.Curves.BLS12_381.G1.Affine.coords
  mkPoint2   = ZK.Algebra.Curves.BLS12_381.G1.Affine.mkPoint

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

foreign import ccall unsafe "bls12_381_G1_affine_is_on_curve" c_bls12_381_G1_affine_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G1 -> Bool
isOnCurve (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_affine_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_affine_is_infinity" c_bls12_381_G1_affine_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G1 -> Bool
isInfinity (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_affine_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_affine_is_in_subgroup" c_bls12_381_G1_affine_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G1 -> Bool
isInSubgroup (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G1_affine_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_affine_is_equal" c_bls12_381_G1_affine_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G1 -> G1 -> Bool
isEqual (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_affine_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_affine_is_same" c_bls12_381_G1_affine_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G1 -> G1 -> Bool
isSame (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_affine_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G1_affine_neg" c_bls12_381_G1_affine_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G1 -> G1
neg (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_affine_neg ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_affine_dbl" c_bls12_381_G1_affine_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G1 -> G1
dbl (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G1_affine_dbl ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bls12_381_G1_affine_add" c_bls12_381_G1_affine_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
add (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr1) (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_affine_add ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_affine_sub" c_bls12_381_G1_affine_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
sub (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr1) (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_affine_sub ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_affine_scl_Fr_mont" c_bls12_381_G1_affine_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
sclFr (MkFr fptr1) (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_affine_scl_Fr_mont ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_affine_scl_big" c_bls12_381_G1_affine_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt384 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
sclBigNonNeg (BigP.MkBigInt384 fptr1) (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_affine_scl_big ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr3)

foreign import ccall unsafe "bls12_381_G1_affine_scl_small" c_bls12_381_G1_affine_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1 -> ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
sclSmallNonNeg k1 (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bls12_381_G1_affine_scl_small (fromIntegral k1) ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G1.Affine.MkG1 fptr3)
