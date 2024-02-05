
-- | BN128 ( Fp2 )  curve, projective coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies, PatternSynonyms #-}
module ZK.Algebra.Curves.BN128.G2.Proj
  ( G2(..)
    -- * Parameters
  , primeR , cofactor , curveA , curveB
  , genG2 , infinity
    -- * Curve points
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
    -- * Conversion to\/from affine
  , fromAffine , toAffine
  , batchFromAffine , batchToAffine
  , normalize
    -- * Predicates
  , isEqual , isSame
  , isOnCurve , isInfinity , isInSubgroup
    -- * Addition and doubling
  , neg , add , madd, dbl , sub
    -- * Scaling
  , sclFr , sclBig , sclSmall
    -- * Random
  , rndG2 , rndG2_naive
    -- * Multi-scalar multiplication
  , msm , msmStd , msmProj
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

import ZK.Algebra.Curves.BN128.Fp.Mont ( Fp(..)  )
import ZK.Algebra.Curves.BN128.Fp2.Mont ( Fp2(..) )
import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr(..)  )
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as Fp
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont as Fp2
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont as Base
import qualified ZK.Algebra.Curves.BN128.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BN128.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt512 as BigP

import {-# SOURCE #-} qualified ZK.Algebra.Curves.BN128.G2.Affine

import           ZK.Algebra.Class.Flat ( FlatArray(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C
import qualified ZK.Algebra.Class.Misc  as M
import           ZK.Algebra.Class.FFT

--------------------------------------------------------------------------------

primeR, cofactor :: Integer
primeR = Fr.prime
cofactor = 21888242871839275222246405745257275088844257914179612981679871602714643921549

type Base = Fp2
pattern MkBase fptr = MkFp2 fptr

-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@
curveA, curveB :: Fp2
curveA = Fp2.pack (0,0)
curveB = Fp2.pack (19485874751759354771024239261021720505790618469301721065564631296452457478373,266929791119991161246907387137283842545076965332900288569378510910307636690)

-- | generator of the r-sized subgroup G1
genG2 :: G2
genG2 = mkPoint (x, y, Fp2.one) where
  x = Fp2.pack (12150282371940648588025820750256225345313886663747209662093324632631129970433,4481220487248307175821185836875007335888437113144368396302892639365759218235)
  y = Fp2.pack (2452391235344852000785071949405254134959145083090483303540469249230552133576,6781711448666880737227463812604550784213209308689191484025779914572346631004)

--------------------------------------------------------------------------------

-- | An elliptic curve point, in projective coordinates
newtype G2 = MkG2 (ForeignPtr Word64)

-- | Note: this throws an exception if the point is not on the curve
mkPoint :: (Base, Base, Base) -> G2
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Base, Base, Base) -> Maybe G2
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

-- | The point at infinity
infinity :: G2
infinity = unsafeMkPoint (Base.zero, Base.one, Base.zero)

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Base, Base, Base) -> G2
unsafeMkPoint (MkBase fptr1 , MkBase fptr2 , MkBase fptr3) = unsafePerformIO $ do
  fptr4 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          copyBytes (        ptr4   ) ptr1 64
          copyBytes (plusPtr ptr4 64) ptr2 64
          copyBytes (plusPtr ptr4 128) ptr3 64
  return (MkG2 fptr4)

{-# NOINLINE coords #-}
coords :: G2 -> (Base, Base, Base)
coords (MkG2 fptr4) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 8
  fptr2 <- mallocForeignPtrArray 8
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          copyBytes ptr1 (        ptr4   ) 64
          copyBytes ptr2 (plusPtr ptr4 64) 64
          copyBytes ptr3 (plusPtr ptr4 128) 64
  return (MkBase fptr1, MkBase fptr2, MkBase fptr3)

-- | Returns a uniformly random element /in the subgroup G2/.
-- Note: this is slow, because it uses exponentiation.
rndG2_naive :: IO G2
rndG2_naive = do
  k <- Fr.rnd :: IO Fr
  return (sclFr k genG2)

-- | Returns a uniformly random element /in the subgroup G2/.
rndG2 :: IO G2
rndG2 = rndG2_naive

--------------------------------------------------------------------------------

instance C.StrictEq G2 where
  (===) = isSame

instance Eq G2 where
  (==) = isEqual
  -- p == q  =  coords (normalize p) == coords (normalize q)

instance Show G2 where
  show pt = case coords pt of
     (x,y,z) -> "[ " ++ show x ++ " : " ++ show y ++ " : " ++ show z ++ " ]"

instance L.Flat G2 where
  sizeInBytes  _pxy = 192
  sizeInQWords _pxy = 24
  withFlat (MkG2 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG2 24

instance M.Rnd G2 where
  rndIO = rndG2

instance C.Group G2 where
  grpName _    = "BN128 / G2 "
  grpIsUnit    = ZK.Algebra.Curves.BN128.G2.Proj.isInfinity
  grpUnit      = ZK.Algebra.Curves.BN128.G2.Proj.infinity
  grpNormalize = normalize
  grpNeg       = neg
  grpDbl       = dbl
  grpAdd       = add
  grpSub       = sub
  grpScale_    = sclSmall
  grpScale     = sclBig

instance C.Curve G2 where
  curveNamePxy _ = "BN128 ( Fp2 ) "
  type BaseField   G2 = Base
  type ScalarField G2 = Fr
  isOnCurve   = ZK.Algebra.Curves.BN128.G2.Proj.isOnCurve
  isInifinity = ZK.Algebra.Curves.BN128.G2.Proj.isInfinity
  infinity    = ZK.Algebra.Curves.BN128.G2.Proj.infinity
  subgroupGen = ZK.Algebra.Curves.BN128.G2.Proj.genG2
  scalarMul   = ZK.Algebra.Curves.BN128.G2.Proj.sclFr
  msm         = ZK.Algebra.Curves.BN128.G2.Proj.msmProj
  curveFFT    = ZK.Algebra.Curves.BN128.G2.Proj.forwardFFT
  curveIFFT   = ZK.Algebra.Curves.BN128.G2.Proj.inverseFFT

instance C.ProjCurve G2 where
  type AffinePoint G2 = ZK.Algebra.Curves.BN128.G2.Affine.G2
  fromAffine = ZK.Algebra.Curves.BN128.G2.Proj.fromAffine
  toAffine   = ZK.Algebra.Curves.BN128.G2.Proj.toAffine
  batchFromAffine = ZK.Algebra.Curves.BN128.G2.Proj.batchFromAffine
  batchToAffine   = ZK.Algebra.Curves.BN128.G2.Proj.batchToAffine
  coords3    = ZK.Algebra.Curves.BN128.G2.Proj.coords
  mkPoint3   = ZK.Algebra.Curves.BN128.G2.Proj.mkPoint
  mixedAdd   = ZK.Algebra.Curves.BN128.G2.Proj.madd
  affMSM     = ZK.Algebra.Curves.BN128.G2.Proj.msm
  
--------------------------------------------------------------------------------

sclSmall :: Int -> G2 -> G2
sclSmall k pt
  | k == 0    = infinity
  | k < 0     = neg $ sclSmallNonNeg (negate k) pt
  | otherwise =       sclSmallNonNeg (       k) pt

sclBig :: Integer -> G2 -> G2
sclBig k pt
  | k == 0    = infinity
  | k < 0     = neg $ sclBigNonNeg (fromInteger $ negate k) pt
  | otherwise =       sclBigNonNeg (fromInteger $        k) pt

--------------------------------------------------------------------------------

msmProj :: FlatArray Fr -> FlatArray G2 -> G2
msmProj cs gs = msm cs (batchToAffine gs)

--------------------------------------------------------------------------------


foreign import ccall unsafe "bn128_G2_proj_MSM_std_coeff_proj_out" c_bn128_G2_proj_MSM_std_coeff_proj_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()
foreign import ccall unsafe "bn128_G2_proj_MSM_mont_coeff_proj_out" c_bn128_G2_proj_MSM_mont_coeff_proj_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

{-# NOINLINE msm #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in Montgomery representation,
-- and the curve points in affine coordinates
-- 
-- > msm :: FlatArray Fr -> FlatArray Affine.G1 -> G1
-- 
msm :: FlatArray Fr -> FlatArray ZK.Algebra.Curves.BN128.G2.Affine.G2 -> G2
msm (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msm: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 24
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G2_proj_MSM_mont_coeff_proj_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG2 fptr3)

{-# NOINLINE msmStd #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in standard representation,
-- and the curve points in affine coordinates
-- 
-- > msmStd :: FlatArray Std.Fr -> FlatArray Affine.G1 -> G1
-- 
msmStd :: FlatArray ZK.Algebra.Curves.BN128.Fr.Std.Fr -> FlatArray ZK.Algebra.Curves.BN128.G2.Affine.G2 -> G2
msmStd (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msm: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 24
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G2_proj_MSM_std_coeff_proj_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG2 fptr3)



foreign import ccall unsafe "bn128_G2_proj_fft_inverse" c_bn128_G2_proj_fft_inverse :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_G2_proj_fft_forward" c_bn128_G2_proj_fft_forward :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE forwardFFT #-}
-- | Forward FFT for groups (converting @[L_k(tau)]@ points to @[tau^i]@ points)
forwardFFT :: FFTSubgroup Fr -> FlatArray G2 -> FlatArray G2
forwardFFT sg (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "forwardNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*24)
      L.withFlat (subgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G2_proj_fft_forward (fromIntegral $ M.fromLog2 $ subgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)

{-# NOINLINE inverseFFT #-}
-- | Inverse FFT for groups (converting @[tau^i]@ points to @[L_k(tau)]@ points)
inverseFFT :: FFTSubgroup Fr -> FlatArray G2 -> FlatArray G2
inverseFFT sg (MkFlatArray n fptr2)
  | subgroupSize sg /= n   = error "inverseNTT: subgroup size differs from the array size"
  | otherwise              = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*24)
      L.withFlat (subgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G2_proj_fft_inverse (fromIntegral $ M.fromLog2 $ subgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)


-- | Sage setup code to experiment with this curve
sageSetup :: [String]
sageSetup = [ "# Sage for G2: TODO" ]

-- | Prints the Sage code
printSageSetup :: IO ()
printSageSetup = mapM_ putStrLn sageSetup

foreign import ccall unsafe "bn128_G2_proj_is_on_curve" c_bn128_G2_proj_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G2 -> Bool
isOnCurve (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_proj_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_proj_is_infinity" c_bn128_G2_proj_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G2 -> Bool
isInfinity (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_proj_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_proj_is_in_subgroup" c_bn128_G2_proj_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G2 -> Bool
isInSubgroup (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_proj_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_proj_is_equal" c_bn128_G2_proj_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G2 -> G2 -> Bool
isEqual (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_proj_is_same" c_bn128_G2_proj_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G2 -> G2 -> Bool
isSame (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_proj_normalize" c_bn128_G2_proj_normalize :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE normalize #-}
normalize :: G2 -> G2
normalize (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_normalize ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_proj_from_affine" c_bn128_G2_proj_from_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE fromAffine #-}
fromAffine :: ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Proj.G2
fromAffine (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_from_affine ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_proj_to_affine" c_bn128_G2_proj_to_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE toAffine #-}
toAffine :: ZK.Algebra.Curves.BN128.G2.Proj.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
toAffine (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_to_affine ptr1 ptr2
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_proj_batch_from_affine" c_bn128_G2_proj_batch_from_affine :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchFromAffine #-}
batchFromAffine :: FlatArray (ZK.Algebra.Curves.BN128.G2.Affine.G2) -> FlatArray (ZK.Algebra.Curves.BN128.G2.Proj.G2)
batchFromAffine (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*24)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_batch_from_affine (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

foreign import ccall unsafe "bn128_G2_proj_batch_to_affine" c_bn128_G2_proj_batch_to_affine :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchToAffine #-}
batchToAffine :: FlatArray (ZK.Algebra.Curves.BN128.G2.Proj.G2) -> FlatArray (ZK.Algebra.Curves.BN128.G2.Affine.G2)
batchToAffine (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*16)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_batch_to_affine (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

foreign import ccall unsafe "bn128_G2_proj_neg" c_bn128_G2_proj_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G2 -> G2
neg (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_neg ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_proj_dbl" c_bn128_G2_proj_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G2 -> G2
dbl (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_proj_dbl ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_proj_add" c_bn128_G2_proj_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: G2 -> G2 -> G2
add (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_proj_add ptr1 ptr2 ptr3
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_proj_sub" c_bn128_G2_proj_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: G2 -> G2 -> G2
sub (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_proj_sub ptr1 ptr2 ptr3
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_proj_madd_proj_aff" c_bn128_G2_proj_madd_proj_aff :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE madd #-}
madd :: G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> G2
madd (MkG2 fptr1) (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_proj_madd_proj_aff ptr1 ptr2 ptr3
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_proj_scl_Fr_mont" c_bn128_G2_proj_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> G2 -> G2
sclFr (MkFr fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_proj_scl_Fr_mont ptr1 ptr2 ptr3
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_proj_scl_big" c_bn128_G2_proj_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt512 -> G2 -> G2
sclBigNonNeg (BigP.MkBigInt512 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_proj_scl_big ptr1 ptr2 ptr3
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_proj_scl_small" c_bn128_G2_proj_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> G2 -> G2
sclSmallNonNeg k1 (MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bn128_G2_proj_scl_small (fromIntegral k1) ptr2 ptr3
  return (MkG2 fptr3)
