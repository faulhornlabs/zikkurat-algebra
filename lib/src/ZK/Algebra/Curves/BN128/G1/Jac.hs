
-- | BN128 curve, Jacobian (or weighted) projective coordinates, Montgomery field representation
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!
--

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies #-}
module ZK.Algebra.Curves.BN128.G1.Jac
  ( G1(..)
  , primeP , primeR , cofactor , curveA , curveB
    -- * Parameters
  , genG1 , infinity
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
  , rndG1 , rndG1_naive
    -- * Multi-scalar multiplication
  , msm , msmStd , msmJac
  , msmBinary , binaryMultiMSM , binaryMultiMSM_
    -- * Fast-Fourier transform
  , forwardFFT , inverseFFT
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

import ZK.Algebra.Curves.BN128.Fp.Mont ( Fp(..) )
import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as Fp
import qualified ZK.Algebra.Curves.BN128.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BN128.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt256 as BigP

import {-# SOURCE #-} qualified ZK.Algebra.Curves.BN128.G1.Affine

import           ZK.Algebra.Class.Flat ( FlatArray(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C
import qualified ZK.Algebra.Class.Misc  as M
import           ZK.Algebra.Class.FFT

import ZK.Algebra.Helpers

--------------------------------------------------------------------------------

-- | The sizes of the fields Fp, Fr, and the cofactor of the subgroup G1
primeP, primeR, cofactor :: Integer
primeP = Fp.prime
primeR = Fr.prime
cofactor = 1

-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@
curveA, curveB :: Fp
curveA = 0
curveB = 3

-- | generator of the r-sized subgroup G1
genG1 :: G1
genG1 = mkPoint (x, y, Fp.one) where
  x = 1
  y = 2

--------------------------------------------------------------------------------

-- | An elliptic curve point, in Jacobian (weighted projective) coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

-- | Note: this throws an exception if the point is not on the curve
mkPoint :: (Fp, Fp, Fp) -> G1
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Fp, Fp, Fp) -> Maybe G1
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

-- | The point at infinity, @{1 : 1 : 0}@
infinity :: G1
infinity = unsafeMkPoint (Fp.one, Fp.one, Fp.zero)

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Fp, Fp, Fp) -> G1
unsafeMkPoint (MkFp fptr1 , MkFp fptr2 , MkFp fptr3) = unsafePerformIO $ do
  fptr4 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          copyBytes (        ptr4   ) ptr1 32
          copyBytes (plusPtr ptr4 32) ptr2 32
          copyBytes (plusPtr ptr4 64) ptr3 32
  return (MkG1 fptr4)

{-# NOINLINE coords #-}
coords :: G1 -> (Fp, Fp, Fp)
coords (MkG1 fptr4) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 4
  fptr2 <- mallocForeignPtrArray 4
  fptr3 <- mallocForeignPtrArray 4
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        withForeignPtr fptr4 $ \ptr4 -> do
          copyBytes ptr1 (        ptr4   ) 32
          copyBytes ptr2 (plusPtr ptr4 32) 32
          copyBytes ptr3 (plusPtr ptr4 64) 32
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
     (x,y,z) -> "{ " ++ show x ++ " : " ++ show y ++ " : " ++ show z ++ " }"

instance L.Flat G1 where
  sizeInBytes  _pxy = 96
  sizeInQWords _pxy = 12
  withFlat (MkG1 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG1 12

instance M.Rnd G1 where
  rndIO = rndG1

instance C.Group G1 where
  grpName _    = "BN128 / G1"
  grpIsUnit    = ZK.Algebra.Curves.BN128.G1.Jac.isInfinity
  grpUnit      = ZK.Algebra.Curves.BN128.G1.Jac.infinity
  grpNormalize = normalize
  grpNeg       = neg
  grpDbl       = dbl
  grpAdd       = add
  grpSub       = sub
  grpScale_    = sclSmall
  grpScale     = sclBig

instance C.Curve G1 where
  curveNamePxy _ = "BN128 ( Fp )"
  type BaseField   G1 = Fp
  type ScalarField G1 = Fr
  isOnCurve   = ZK.Algebra.Curves.BN128.G1.Jac.isOnCurve
  isInfinity  = ZK.Algebra.Curves.BN128.G1.Jac.isInfinity
  infinity    = ZK.Algebra.Curves.BN128.G1.Jac.infinity
  curveSubgroupGen = ZK.Algebra.Curves.BN128.G1.Jac.genG1
  scalarMul   = ZK.Algebra.Curves.BN128.G1.Jac.sclFr
  msm         = ZK.Algebra.Curves.BN128.G1.Jac.msmJac
  curveFFT    = ZK.Algebra.Curves.BN128.G1.Jac.forwardFFT
  curveIFFT   = ZK.Algebra.Curves.BN128.G1.Jac.inverseFFT

instance C.ProjCurve G1 where
  type AffinePoint G1 = ZK.Algebra.Curves.BN128.G1.Affine.G1
  fromAffine = ZK.Algebra.Curves.BN128.G1.Jac.fromAffine
  batchFromAffine = ZK.Algebra.Curves.BN128.G1.Jac.batchFromAffine
  batchToAffine   = ZK.Algebra.Curves.BN128.G1.Jac.batchToAffine
  toAffine   = ZK.Algebra.Curves.BN128.G1.Jac.toAffine
  coords3    = ZK.Algebra.Curves.BN128.G1.Jac.coords
  mkPoint3   = ZK.Algebra.Curves.BN128.G1.Jac.mkPoint
  mixedAdd   = ZK.Algebra.Curves.BN128.G1.Jac.madd
  affMSM     = ZK.Algebra.Curves.BN128.G1.Jac.msm
  binaryMSM       = ZK.Algebra.Curves.BN128.G1.Jac.msmBinary
  binaryMultiMSM  = ZK.Algebra.Curves.BN128.G1.Jac.binaryMultiMSM
  
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

msmJac :: FlatArray Fr -> FlatArray G1 -> G1
msmJac cs gs = msm cs (batchToAffine gs)

--------------------------------------------------------------------------------


foreign import ccall unsafe "bn128_G1_jac_MSM_std_coeff_jac_out" c_bn128_G1_jac_MSM_std_coeff_jac_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()
foreign import ccall unsafe "bn128_G1_jac_MSM_mont_coeff_jac_out" c_bn128_G1_jac_MSM_mont_coeff_jac_out :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()
foreign import ccall unsafe "bn128_G1_jac_MSM_binary_coeff_jac_out" c_bn128_G1_jac_MSM_binary_coeff_jac_out :: CInt -> Ptr Word8  -> Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bn128_G1_jac_multi_MSM_binary_coeff_jac_out" c_bn128_G1_jac_multi_MSM_binary_coeff_jac_out :: CInt -> CInt -> Ptr (Ptr Word8) -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_G1_jac_multi_MSM_binary_coeff_affine_out" c_bn128_G1_jac_multi_MSM_binary_coeff_affine_out :: CInt -> CInt -> Ptr (Ptr Word8) -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE msm #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in Montgomery representation,
-- and the curve points in affine coordinates
-- 
-- > msm :: FlatArray Fr -> FlatArray Affine.G1 -> G1
-- 
msm :: FlatArray Fr -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1 -> G1
msm (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msm: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 12
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G1_jac_MSM_mont_coeff_jac_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG1 fptr3)

{-# NOINLINE msmStd #-}
-- | Multi-Scalar Multiplication (MSM), with the coefficients in standard representation,
-- and the curve points in affine coordinates
-- 
-- > msmStd :: FlatArray Std.Fr -> FlatArray Affine.G1 -> G1
-- 
msmStd :: FlatArray ZK.Algebra.Curves.BN128.Fr.Std.Fr -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1 -> G1
msmStd (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msmStd: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 12
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G1_jac_MSM_std_coeff_jac_out (fromIntegral n1) ptr1 ptr2 ptr3 4
      return (MkG1 fptr3)

{-# NOINLINE msmBinary #-}
-- | Multi-Scalar Multiplication (MSM), with binary coefficients,
-- and the curve points in affine coordinates
-- 
-- > msmBinary :: FlatArray Bit -> FlatArray Affine.G1 -> G1
-- 
msmBinary :: FlatArray L.Bit -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1 -> G1
msmBinary (MkFlatArray n1 fptr1) (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "msmBinary: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray 12
      withForeignPtr fptr1 $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G1_jac_MSM_binary_coeff_jac_out (fromIntegral n1) (castPtr ptr1) ptr2 ptr3
      return (MkG1 fptr3)

{-# NOINLINE binaryMultiMSM #-}
-- | Multiple binary MSM-s with binary coefficients,
-- and the curve points in affine coordinates
-- 
-- > binaryMultiMSM :: [FlatArray Bit] -> FlatArray Affine.G1 -> FlatArray G1
-- 
-- This can be more efficient (up to 2x or more), based on the number of coefficient arrays - 
-- the suggested minimum is 32 - than calling 'msmBinary' multiple times
-- 
-- Note: it is assumed that all the coefficient arrays have the same length!
binaryMultiMSM :: [FlatArray L.Bit] -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1 -> FlatArray G1
binaryMultiMSM flatarrs (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "binaryMultiMSM: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (narrs * 12)
      L.withFlatArrays flatarrs $ \list -> do
        let ptrs = map (castPtr . snd) list :: [Ptr Word8]
        withArray ptrs $ \ptr_ptrs -> do
          withForeignPtr fptr2 $ \ptr2 -> do
            withForeignPtr fptr3 $ \ptr3 -> do
              c_bn128_G1_jac_multi_MSM_binary_coeff_jac_out (fromIntegral narrs) (fromIntegral n1) ptr_ptrs ptr2 ptr3
      return (MkFlatArray narrs fptr3)
  where
    narrs = length flatarrs
    ns    = map L.flatArrayLength flatarrs
    n1    = case maybeAllIsEqual ns of
      Just m  -> m
      Nothing -> error "binaryMultiMSM: incompatible coefficient array dimensions"

{-# NOINLINE binaryMultiMSM_ #-}
-- | Multiple binary MSM-s with binary coefficients, with affine output
-- and the curve points in affine coordinates
-- 
-- > binaryMultiMSM_ :: [FlatArray Bit] -> FlatArray Affine.G1 -> FlatArray Affine.G1
-- 
binaryMultiMSM_ :: [FlatArray L.Bit] -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1 -> FlatArray ZK.Algebra.Curves.BN128.G1.Affine.G1
binaryMultiMSM_ flatarrs (MkFlatArray n2 fptr2)
  | n1 /= n2   = error "binaryMultiMSM: incompatible array dimensions"
  | otherwise  = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (narrs * 8)
      L.withFlatArrays flatarrs $ \list -> do
        let ptrs = map (castPtr . snd) list :: [Ptr Word8]
        withArray ptrs $ \ptr_ptrs -> do
          withForeignPtr fptr2 $ \ptr2 -> do
            withForeignPtr fptr3 $ \ptr3 -> do
              c_bn128_G1_jac_multi_MSM_binary_coeff_affine_out (fromIntegral narrs) (fromIntegral n1) ptr_ptrs ptr2 ptr3
      return (MkFlatArray narrs fptr3)
  where
    narrs = length flatarrs
    ns    = map L.flatArrayLength flatarrs
    n1    = case maybeAllIsEqual ns of
      Just m  -> m
      Nothing -> error "binaryMultiMSM_: incompatible coefficient array dimensions"



foreign import ccall unsafe "bn128_G1_jac_fft_inverse" c_bn128_G1_jac_fft_inverse :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_G1_jac_fft_forward" c_bn128_G1_jac_fft_forward :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE forwardFFT #-}
-- | Forward FFT for groups (converting @[L_k(tau)]@ points to @[tau^i]@ points)
forwardFFT :: FFTSubgroup Fr -> FlatArray G1 -> FlatArray G1
forwardFFT sg (MkFlatArray n fptr2)
  | fftSubgroupSize sg /= n   = error "forwardNTT: subgroup size differs from the array size"
  | otherwise                 = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*12)
      L.withFlat (fftSubgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G1_jac_fft_forward (fromIntegral $ M.fromLog2 $ fftSubgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)

{-# NOINLINE inverseFFT #-}
-- | Inverse FFT for groups (converting @[tau^i]@ points to @[L_k(tau)]@ points)
inverseFFT :: FFTSubgroup Fr -> FlatArray G1 -> FlatArray G1
inverseFFT sg (MkFlatArray n fptr2)
  | fftSubgroupSize sg /= n   = error "inverseNTT: subgroup size differs from the array size"
  | otherwise                 = unsafePerformIO $ do
      fptr3 <- mallocForeignPtrArray (n*12)
      L.withFlat (fftSubgroupGen sg) $ \ptr1 -> do
        withForeignPtr fptr2 $ \ptr2 -> do
          withForeignPtr fptr3 $ \ptr3 -> do
            c_bn128_G1_jac_fft_inverse (fromIntegral $ M.fromLog2 $ fftSubgroupLogSize sg) ptr1 ptr2 ptr3
      return (MkFlatArray n fptr3)


foreign import ccall unsafe "bn128_G1_jac_is_on_curve" c_bn128_G1_jac_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G1 -> Bool
isOnCurve (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G1_jac_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G1_jac_is_infinity" c_bn128_G1_jac_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G1 -> Bool
isInfinity (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G1_jac_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G1_jac_is_in_subgroup" c_bn128_G1_jac_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G1 -> Bool
isInSubgroup (MkG1 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G1_jac_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G1_jac_is_equal" c_bn128_G1_jac_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G1 -> G1 -> Bool
isEqual (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G1_jac_is_same" c_bn128_G1_jac_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G1 -> G1 -> Bool
isSame (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G1_jac_normalize" c_bn128_G1_jac_normalize :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE normalize #-}
normalize :: G1 -> G1
normalize (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_normalize ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bn128_G1_jac_from_affine" c_bn128_G1_jac_from_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE fromAffine #-}
fromAffine :: ZK.Algebra.Curves.BN128.G1.Affine.G1 -> ZK.Algebra.Curves.BN128.G1.Jac.G1
fromAffine (ZK.Algebra.Curves.BN128.G1.Affine.MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_from_affine ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bn128_G1_jac_to_affine" c_bn128_G1_jac_to_affine :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE toAffine #-}
toAffine :: ZK.Algebra.Curves.BN128.G1.Jac.G1 -> ZK.Algebra.Curves.BN128.G1.Affine.G1
toAffine (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_to_affine ptr1 ptr2
  return (ZK.Algebra.Curves.BN128.G1.Affine.MkG1 fptr2)

foreign import ccall unsafe "bn128_G1_jac_batch_from_affine" c_bn128_G1_jac_batch_from_affine :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchFromAffine #-}
batchFromAffine :: FlatArray (ZK.Algebra.Curves.BN128.G1.Affine.G1) -> FlatArray (ZK.Algebra.Curves.BN128.G1.Jac.G1)
batchFromAffine (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*12)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_batch_from_affine (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

foreign import ccall unsafe "bn128_G1_jac_batch_to_affine" c_bn128_G1_jac_batch_to_affine :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchToAffine #-}
batchToAffine :: FlatArray (ZK.Algebra.Curves.BN128.G1.Jac.G1) -> FlatArray (ZK.Algebra.Curves.BN128.G1.Affine.G1)
batchToAffine (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*8)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_batch_to_affine (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

foreign import ccall unsafe "bn128_G1_jac_neg" c_bn128_G1_jac_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G1 -> G1
neg (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_neg ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bn128_G1_jac_dbl" c_bn128_G1_jac_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G1 -> G1
dbl (MkG1 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G1_jac_dbl ptr1 ptr2
  return (MkG1 fptr2)

foreign import ccall unsafe "bn128_G1_jac_add" c_bn128_G1_jac_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: G1 -> G1 -> G1
add (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G1_jac_add ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bn128_G1_jac_sub" c_bn128_G1_jac_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: G1 -> G1 -> G1
sub (MkG1 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G1_jac_sub ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bn128_G1_jac_madd_jac_aff" c_bn128_G1_jac_madd_jac_aff :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE madd #-}
madd :: G1 -> ZK.Algebra.Curves.BN128.G1.Affine.G1 -> G1
madd (MkG1 fptr1) (ZK.Algebra.Curves.BN128.G1.Affine.MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G1_jac_madd_jac_aff ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bn128_G1_jac_scl_Fr_mont" c_bn128_G1_jac_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> G1 -> G1
sclFr (MkFr fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G1_jac_scl_Fr_mont ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bn128_G1_jac_scl_big" c_bn128_G1_jac_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt256 -> G1 -> G1
sclBigNonNeg (BigP.MkBigInt256 fptr1) (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G1_jac_scl_big ptr1 ptr2 ptr3
  return (MkG1 fptr3)

foreign import ccall unsafe "bn128_G1_jac_scl_small" c_bn128_G1_jac_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> G1 -> G1
sclSmallNonNeg k1 (MkG1 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 12
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bn128_G1_jac_scl_small (fromIntegral k1) ptr2 ptr3
  return (MkG1 fptr3)
