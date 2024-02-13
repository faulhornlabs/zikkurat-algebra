
-- | BN128 ( Fp2 )  curve, affine coordinates, Montgomery field representation

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies, PatternSynonyms #-}
module ZK.Algebra.Curves.BN128.G2.Affine
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
    -- * handling infinities
  , convertInfinityIO
  , batchConvertInfinityIO
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
import qualified ZK.Algebra.Curves.BN128.Fr.Mont as Fr
import qualified ZK.Algebra.Curves.BN128.Fr.Std
import qualified ZK.Algebra.BigInt.BigInt512 as BigP

import qualified ZK.Algebra.Curves.BN128.G2.Proj as Proj    -- note: be careful with cyclic imports!

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
genG2 = mkPoint (x, y) where
  x = Fp2.pack (12150282371940648588025820750256225345313886663747209662093324632631129970433,4481220487248307175821185836875007335888437113144368396302892639365759218235)
  y = Fp2.pack (2452391235344852000785071949405254134959145083090483303540469249230552133576,6781711448666880737227463812604550784213209308689191484025779914572346631004)

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
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes (        ptr3   ) ptr1 64
        copyBytes (plusPtr ptr3 64) ptr2 64
  return (MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_affine_set_infinity" c_bn128_G2_affine_set_infinity :: Ptr Word64 -> IO ()

-- | The point at infinity (represented as the special string @0xffff...ffff@)
{-# NOINLINE infinity #-}
infinity :: G2
infinity = unsafePerformIO $ do
  fptr <- mallocForeignPtrArray 16
  withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_affine_set_infinity ptr
  return (MkG2 fptr)

{-# NOINLINE coords #-}
-- | Affine coordinates (TODO: handle the point at infinity)
coords :: G2 -> (Base, Base)
coords (MkG2 fptr3) = unsafePerformIO $ do
  fptr1 <- mallocForeignPtrArray 8
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        copyBytes ptr1 (        ptr3   ) 64
        copyBytes ptr2 (plusPtr ptr3 64) 64
  return (MkBase fptr1, MkBase fptr2)

-- | Returns a uniformly random element /in the subgroup G2/
rndG2 :: IO G2
rndG2 = Proj.toAffine <$> Proj.rndG2

--------------------------------------------------------------------------------

-- | Multi-Scalar Multiplication (MSM), with the coefficients in Montgomery representation
msm :: L.FlatArray Fr -> L.FlatArray ZK.Algebra.Curves.BN128.G2.Affine.G2 -> G2
msm cs gs = Proj.toAffine $ Proj.msm cs gs

-- | Multi-Scalar Multiplication (MSM), with the coefficients in standard representation
msmStd :: L.FlatArray ZK.Algebra.Curves.BN128.Fr.Std.Fr -> L.FlatArray ZK.Algebra.Curves.BN128.G2.Affine.G2 -> G2
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
  sizeInBytes  _pxy = 128
  sizeInQWords _pxy = 16
  withFlat (MkG2 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkG2 16

instance M.Rnd G2 where
  rndIO = rndG2

instance C.Group G2 where
  grpName _    = "BN128 / G2 "
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
  curveNamePxy _ = "BN128 ( Fp2 ) "
  type BaseField   G2 = Base
  type ScalarField G2 = Fr
  isOnCurve   = ZK.Algebra.Curves.BN128.G2.Affine.isOnCurve
  isInfinity  = ZK.Algebra.Curves.BN128.G2.Affine.isInfinity
  infinity    = ZK.Algebra.Curves.BN128.G2.Affine.infinity
  curveSubgroupGen = ZK.Algebra.Curves.BN128.G2.Affine.genG2
  scalarMul   = ZK.Algebra.Curves.BN128.G2.Affine.sclFr
  msm         = ZK.Algebra.Curves.BN128.G2.Affine.msm
  curveFFT    = ZK.Algebra.Curves.BN128.G2.Affine.forwardFFT
  curveIFFT   = ZK.Algebra.Curves.BN128.G2.Affine.inverseFFT

instance C.AffineCurve G2 where
  coords2    = ZK.Algebra.Curves.BN128.G2.Affine.coords
  mkPoint2   = ZK.Algebra.Curves.BN128.G2.Affine.mkPoint
  convertInfinityIO = ZK.Algebra.Curves.BN128.G2.Affine.convertInfinityIO
  batchConvertInfinityIO = ZK.Algebra.Curves.BN128.G2.Affine.batchConvertInfinityIO

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

foreign import ccall unsafe "bn128_G2_affine_convert_infinity_inplace" c_bn128_G2_affine_convert_infinity_inplace :: Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_G2_affine_batch_convert_infinity_inplace" c_bn128_G2_affine_batch_convert_infinity_inplace :: CInt -> Ptr Word64 -> IO ()

convertInfinityIO :: G2 -> IO ()
convertInfinityIO (MkG2 fptr) = do
  withForeignPtr fptr $ \ptr -> c_bn128_G2_affine_convert_infinity_inplace ptr

batchConvertInfinityIO :: L.FlatArray G2 -> IO ()
batchConvertInfinityIO (L.MkFlatArray n fptr) = do
  withForeignPtr fptr $ \ptr -> c_bn128_G2_affine_batch_convert_infinity_inplace (fromIntegral n) ptr

--------------------------------------------------------------------------------


-- | Sage setup code to experiment with this curve
sageSetup :: [String]
sageSetup = [ "# Sage for G2: TODO" ]

-- | Prints the Sage code
printSageSetup :: IO ()
printSageSetup = mapM_ putStrLn sageSetup

foreign import ccall unsafe "bn128_G2_affine_is_on_curve" c_bn128_G2_affine_is_on_curve :: Ptr Word64 -> IO Word8

{-# NOINLINE isOnCurve #-}
isOnCurve :: G2 -> Bool
isOnCurve (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_affine_is_on_curve ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_affine_is_infinity" c_bn128_G2_affine_is_infinity :: Ptr Word64 -> IO Word8

{-# NOINLINE isInfinity #-}
isInfinity :: G2 -> Bool
isInfinity (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_affine_is_infinity ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_affine_is_in_subgroup" c_bn128_G2_affine_is_in_subgroup :: Ptr Word64 -> IO Word8

{-# NOINLINE isInSubgroup #-}
isInSubgroup :: G2 -> Bool
isInSubgroup (MkG2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_G2_affine_is_in_subgroup ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_affine_is_equal" c_bn128_G2_affine_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: G2 -> G2 -> Bool
isEqual (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_affine_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_affine_is_same" c_bn128_G2_affine_is_same :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isSame #-}
isSame :: G2 -> G2 -> Bool
isSame (MkG2 fptr1) (MkG2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_affine_is_same ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_G2_affine_neg" c_bn128_G2_affine_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: G2 -> G2
neg (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_affine_neg ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_affine_dbl" c_bn128_G2_affine_dbl :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE dbl #-}
dbl :: G2 -> G2
dbl (MkG2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_G2_affine_dbl ptr1 ptr2
  return (MkG2 fptr2)

foreign import ccall unsafe "bn128_G2_affine_add" c_bn128_G2_affine_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
add (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr1) (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_affine_add ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_affine_sub" c_bn128_G2_affine_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
sub (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr1) (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_affine_sub ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_affine_scl_Fr_mont" c_bn128_G2_affine_scl_Fr_mont :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclFr #-}
sclFr :: Fr -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
sclFr (MkFr fptr1) (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_affine_scl_Fr_mont ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_affine_scl_big" c_bn128_G2_affine_scl_big :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclBigNonNeg #-}
sclBigNonNeg :: BigP.BigInt512 -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
sclBigNonNeg (BigP.MkBigInt512 fptr1) (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_G2_affine_scl_big ptr1 ptr2 ptr3
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr3)

foreign import ccall unsafe "bn128_G2_affine_scl_small" c_bn128_G2_affine_scl_small :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sclSmallNonNeg #-}
sclSmallNonNeg :: Int -> ZK.Algebra.Curves.BN128.G2.Affine.G2 -> ZK.Algebra.Curves.BN128.G2.Affine.G2
sclSmallNonNeg k1 (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 16
  withForeignPtr fptr2 $ \ptr2 -> do
    withForeignPtr fptr3 $ \ptr3 -> do
      c_bn128_G2_affine_scl_small (fromIntegral k1) ptr2 ptr3
  return (ZK.Algebra.Curves.BN128.G2.Affine.MkG2 fptr3)
