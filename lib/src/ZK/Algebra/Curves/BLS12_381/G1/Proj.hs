
-- | BLS12-381 ( Fp )  curve, projective coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies, PatternSynonyms #-}
module ZK.Algebra.Curves.BLS12_381.G1.Proj
  ( G1(..)
    -- * Parameters
  , primeP , primeR , cofactor , curveA , curveB
  , genG1 , infinity
    -- * Curve points
    -- * Curve points
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
  , fromAffine , toAffine
  , normalize
    -- * Predicates
  , isEqual , isSame
  , isOnCurve , isInfinity , isInSubgroup
    -- * Addition and doubling
  , neg , add , madd, dbl , sub
    -- * Scaling
  , sclFr , sclBig , sclSmall
    -- * Random
  , rndG1 , rndG1_naive
    -- * Multi-scalar multiplication
  , msm , msmStd
    -- * Fast-Fourier transform
  , forwardFFT , inverseFFT
    -- * Sage
  , sageSetup , printSageSetup
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
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont as Base
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt384 as BigP

import {-# SOURCE #-} qualified ZK.Algebra.Curves.BLS12_381.G1.Affine

import           ZK.Algebra.Class.Flat ( FlatArray(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C
import           ZK.Algebra.Class.FFT

--------------------------------------------------------------------------------

primeP, primeR, cofactor :: Integer
primeP = Fp.prime
primeR = Fr.prime
cofactor = 76329603384216526031706109802092473003

type Base = Fp
pattern MkBase fptr = MkFp fptr

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
mkPoint :: (Base, Base, Base) -> G1
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Base, Base, Base) -> Maybe G1
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

-- | The point at infinity
infinity :: G1
infinity = unsafeMkPoint (Base.zero, Base.one, Base.zero)

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Base, Base, Base) -> G1
unsafeMkPoint (MkBase fptr1 , MkBase fptr2 , MkBase fptr3) = unsafePerformIO $ do
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
coords :: G1 -> (Base, Base, Base)
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
  return (MkBase fptr1, MkBase fptr2, MkBase fptr3)

-- | Returns a uniformly random element /in the subgroup G1/.
-- Note: this is slow, because it uses exponentiation.
rndG1_naive :: IO G1
rndG1_naive = do
  k <- Fr.rnd :: IO Fr
  return (sclFr k genG1)

-- | Returns a uniformly random element /in the subgroup G1/.
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
  withFlat (MkG1 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG1 18

instance F.Rnd G1 where
  rndIO = rndG1

instance C.Group G1 where
  grpName _    = "BLS12-381 / G1 "
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
  curveNamePxy _ = "BLS12-381 ( Fp ) "
  type BaseField   G1 = Base
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
            c_bls12_381_G1_proj_MSM_std_coeff_proj_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG1 fptr3)



foreign import ccall unsafe "bls12_381_G1_proj_fft_inverse" c_bls12_381_G1_proj_fft_inverse :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_G1_proj_fft_forward" c_bls12_381_G1_proj_fft_forward :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE forwardFFT #-}
-- | Forward FFT for groups (converting @[L_k(tau)]@ points to @[tau^i]@ points)
forwardFFT :: FFTSubgroup Fr -> FlatArray G1 -> FlatArray G1
forwardFFT sg (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "forwardNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*18)
      L.withFlat (subgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bls12_381_G1_proj_fft_forward (fromIntegral $ subgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)

{-# NOINLINE inverseFFT #-}
-- | Inverse FFT for groups (converting @[tau^i]@ points to @[L_k(tau)]@ points)
inverseFFT :: FFTSubgroup Fr -> FlatArray G1 -> FlatArray G1
inverseFFT sg (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "inverseNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*18)
      L.withFlat (subgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bls12_381_G1_proj_fft_inverse (fromIntegral $ subgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)


-- | Sage setup code to experiment with this curve
sageSetup :: [String]
sageSetup = 
  [ "# BLS12-381 elliptic curve"
  , "p  = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
  , "r  = 52435875175126190479447740508185965837690552500527637822603658699938581184513"
  , "h  = 76329603384216526031706109802092473003"
  , "Fp = GF(p)"
  , "Fr = GF(r)"
  , "A  = Fp(0)"
  , "B  = Fp(4)"
  , "E  = EllipticCurve(Fp,[A,B])"
  , "gx = Fp(3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507)"
  , "gy = Fp(1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569)"
  , "gen = E(gx,gy)  # subgroup generator"
  , "print(\"scalar field check: \", gen.additive_order() == r )"
  , "print(\"cofactor check:     \", E.cardinality() == r*h )"
  , ""
  , "# GLV beta and gamma parameters"
  , "beta = Fp(4002409555221667392624310435006688643935503118305586438271171395842971157480381377015405980053539358417135540939436)"
  , "lam  = 228988810152649578064853576960394133503"
  , "pt   = 1234567 * gen;"
  , "pt2  = E( beta*pt[0] , pt[1], pt[2] )"
  , "print(\"beta check:   \", beta^3 == 1 )"
  , "print(\"lambda check: \", Fr(lam)^3 == 1 )"
  , "print(\"GLV check:    \", lam * pt == pt2 )"
  ]

-- | Prints the Sage code
printSageSetup :: IO ()
printSageSetup = mapM_ putStrLn sageSetup

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

foreign import ccall unsafe "bls12_381_G1_proj_scl_Fr_mont" c_bls12_381_G1_proj_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> G1 -> G1
sclFr (MkFr fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 18
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G1_proj_scl_Fr_mont ptr1 ptr2 ptr3
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
