
-- | BLS12-381 ( Fp2 )  curve, affine coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies, PatternSynonyms #-}
module ZK.Algebra.Curves.BLS12_381.G2.Affine
  ( G2(..)
    -- * Parameters
  , primeR , cofactor , curveA , curveB
  , genG2 , infinity
    -- * Curve points
  , coords , mkPoint , mkPointMaybe , unsafeMkPoint
    -- * Predicates
  , isEqual , isSame
  , isInfinity , isOnCurve , isInSubgroup
    -- * Addition and doubling
  , neg , add , dbl , sub
    -- * Scaling
  , sclFr , sclBig , sclSmall
    -- * Random
  , rndG2
    -- * Multi-scalar-multiplication
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

import ZK.Algebra.Curves.BLS12_381.Fp.Mont ( Fp(..)  )
import ZK.Algebra.Curves.BLS12_381.Fp2.Mont ( Fp2(..) )
import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr(..)  )
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont as Fp
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont as Fp2
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt768 as BigP

import qualified ZK.Algebra.Curves.BLS12_381.G2.Proj as Proj    -- note: be careful with cyclic imports!

import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C
import qualified ZK.Algebra.Class.Misc  as M
import           ZK.Algebra.Class.FFT

--------------------------------------------------------------------------------

primeR, cofactor :: Integer
primeR = Fr.prime
cofactor = 305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041616661285803823378372096355777062779109

type Base = Fp2
pattern MkBase fptr = MkFp2 fptr

-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@
curveA, curveB :: Fp2
curveA = Fp2.pack (0,0)
curveB = Fp2.pack (4,4)

-- | generator of the r-sized subgroup G1
genG2 :: G2
genG2 = mkPoint (x, y) where
  x = Fp2.pack (352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160,3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758)
  y = Fp2.pack (1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905,927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582)

--------------------------------------------------------------------------------

-- | An elliptic curve point, in affine coordinates
newtype G2 = MkG2 (ForeignPtr Word64)

-- | Note: this throws an exception if the point is not on the curve
mkPoint :: (Base, Base) -> G2
mkPoint xyz = case mkPointMaybe xyz of
  Just pt -> pt
  Nothing -> error "mkPoint: point is not on the curve"

mkPointMaybe :: (Base, Base) -> Maybe G2
mkPointMaybe xyz = let pt = unsafeMkPoint xyz in
  case isOnCurve pt of { True -> Just pt ; False -> Nothing }

{-# NOINLINE unsafeMkPoint #-}
unsafeMkPoint :: (Base, Base) -> G2
unsafeMkPoint (MkBase fptr1 , MkBase fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes (        ptr3   ) ptr1 96
        copyBytes (plusPtr ptr3 96) ptr2 96
  return (MkG2 fptr3)

foreign import ccall unsafe "bls12_381_G2_affine_set_infinity" c_bls12_381_G2_affine_set_infinity :: Ptr Word64 -> IO ()

-- | The point at infinity (represented as the special string @0xffff...ffff@)
{-# NOINLINE infinity #-}
infinity :: G2
infinity = unsafePerformIO $ do
  fptr <- mallocForeignPtrArray 24
  withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G2_affine_set_infinity ptr
  return (MkG2 fptr)

{-# NOINLINE coords #-}
-- | Affine coordinates (TODO: handle the point at infinity)
coords :: G2 -> (Base, Base)
coords (MkG2 fptr3) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 12
  fptr2 <- mallocForeignPtrArray 12
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes ptr1 (        ptr3   ) 96
        copyBytes ptr2 (plusPtr ptr3 96) 96
  return (MkBase fptr1, MkBase fptr2)

-- | Returns a uniformly random element /in the subgroup G2/
rndG2 :: IO G2
rndG2 = Proj.toAffine <$> Proj.rndG2

--------------------------------------------------------------------------------

-- | Multi-Scalar Multiplication (MSM), with the coefficients in Montgomery representation
msm :: L.FlatArray Fr -> L.FlatArray ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> G2
msm cs gs = Proj.toAffine $ Proj.msm cs gs

-- | Multi-Scalar Multiplication (MSM), with the coefficients in standard representation
msmStd :: L.FlatArray ZK.Algebra.Curves.BLS12_381.Fr.Std.Fr -> L.FlatArray ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> G2
msmStd cs gs = Proj.toAffine $ Proj.msmStd cs gs

-- | Forward FFT for groups (converting @[L_k(tau)]@ points to @[tau^i]@ points)
forwardFFT :: FFTSubgroup Fr -> L.FlatArray G2 -> L.FlatArray G2
forwardFFT sg = Proj.batchToAffine . Proj.forwardFFT sg . Proj.batchFromAffine

-- | Inverse FFT for groups (converting @[tau^i]@ points to @[L_k(tau)]@ points)
inverseFFT :: FFTSubgroup Fr -> L.FlatArray G2 -> L.FlatArray G2
inverseFFT sg = Proj.batchToAffine . Proj.inverseFFT sg . Proj.batchFromAffine

--------------------------------------------------------------------------------

instance C.StrictEq G2 where
  (===) = isSame

instance Eq G2 where
  (==) = isEqual

instance Show G2 where
  show pt
    | isInfinity pt = "<point-at-infinity>"
    | otherwise = case coords pt of
        (x,y) -> "( " ++ show x ++ " , " ++ show y ++ " )"

instance L.Flat G2 where
  sizeInBytes  _pxy = 192
  sizeInQWords _pxy = 24
  withFlat (MkG2 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG2 24

instance M.Rnd G2 where
  rndIO = rndG2

instance C.Group G2 where
  grpName _    = "BLS12-381 / G2 "
  grpIsUnit    = isInfinity
  grpUnit      = infinity
  grpNormalize = id
  grpNeg       = neg
  grpDbl       = dbl
  grpAdd       = add
  grpSub       = sub
  grpScale_    = sclSmall
  grpScale     = sclBig

instance C.Curve G2 where
  curveNamePxy _ = "BLS12-381 ( Fp2 ) "
  type BaseField   G2 = Base
  type ScalarField G2 = Fr
  isOnCurve   = ZK.Algebra.Curves.BLS12_381.G2.Affine.isOnCurve
  isInifinity = ZK.Algebra.Curves.BLS12_381.G2.Affine.isInfinity
  infinity    = ZK.Algebra.Curves.BLS12_381.G2.Affine.infinity
  curveSubgroupGen = ZK.Algebra.Curves.BLS12_381.G2.Affine.genG2
  scalarMul   = ZK.Algebra.Curves.BLS12_381.G2.Affine.sclFr
  msm         = ZK.Algebra.Curves.BLS12_381.G2.Affine.msm
  curveFFT    = ZK.Algebra.Curves.BLS12_381.G2.Affine.forwardFFT
  curveIFFT   = ZK.Algebra.Curves.BLS12_381.G2.Affine.inverseFFT

instance C.AffineCurve G2 where
  coords2    = ZK.Algebra.Curves.BLS12_381.G2.Affine.coords
  mkPoint2   = ZK.Algebra.Curves.BLS12_381.G2.Affine.mkPoint

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

-- | Sage setup code to experiment with this curve
sageSetup :: [String]
sageSetup = [ "# Sage for G2: TODO" ]

-- | Prints the Sage code
printSageSetup :: IO ()
printSageSetup = mapM_ putStrLn sageSetup

foreign import ccall unsafe "bls12_381_G2_affine_is_on_curve" c_bls12_381_G2_affine_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G2 -> Bool
isOnCurve (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G2_affine_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G2_affine_is_infinity" c_bls12_381_G2_affine_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G2 -> Bool
isInfinity (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G2_affine_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G2_affine_is_in_subgroup" c_bls12_381_G2_affine_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G2 -> Bool
isInSubgroup (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_G2_affine_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G2_affine_is_equal" c_bls12_381_G2_affine_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G2 -> G2 -> Bool
isEqual (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G2_affine_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G2_affine_is_same" c_bls12_381_G2_affine_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G2 -> G2 -> Bool
isSame (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G2_affine_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_G2_affine_neg" c_bls12_381_G2_affine_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G2 -> G2
neg (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G2_affine_neg ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bls12_381_G2_affine_dbl" c_bls12_381_G2_affine_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G2 -> G2
dbl (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_G2_affine_dbl ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bls12_381_G2_affine_add" c_bls12_381_G2_affine_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
add (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr1) (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G2_affine_add ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bls12_381_G2_affine_sub" c_bls12_381_G2_affine_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
sub (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr1) (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G2_affine_sub ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bls12_381_G2_affine_scl_Fr_mont" c_bls12_381_G2_affine_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
sclFr (MkFr fptr1) (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G2_affine_scl_Fr_mont ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bls12_381_G2_affine_scl_big" c_bls12_381_G2_affine_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt768 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
sclBigNonNeg (BigP.MkBigInt768 fptr1) (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_G2_affine_scl_big ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bls12_381_G2_affine_scl_small" c_bls12_381_G2_affine_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2 -> ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
sclSmallNonNeg k1 (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bls12_381_G2_affine_scl_small (fromIntegral k1) ptr2 ptr3
  return (ZK.Algebra.Curves.BLS12_381.G2.Affine.MkG2 fptr3)
