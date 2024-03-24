
-- | Projective coordinates, Montgomery field representation

{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.MontProj where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc
--import Zikkurat.Primes -- ( integerLog2 )

import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Curve.CurveFFI
import Zikkurat.CodeGen.Curve.Shared
import Zikkurat.CodeGen.Curve.MSM
import Zikkurat.CodeGen.Curve.FFT

--------------------------------------------------------------------------------

c_header :: CodeGenParams -> Code
c_header cgparams@(CodeGenParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "normalize         ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "normalize_inplace (       uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "copy        ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "from_affine ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "to_affine   ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "batch_from_affine( int N, const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "batch_to_affine  ( int N, const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_on_curve   ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_infinity   ( const uint64_t *src );"
  , "extern void    " ++ prefix ++ "set_infinity  (       uint64_t *tgt );"
  , "extern uint8_t " ++ prefix ++ "is_in_subgroup( const uint64_t *src );"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_equal( const uint64_t *src1, const uint64_t *src2 );"
  , "extern uint8_t " ++ prefix ++ "is_same ( const uint64_t *src1, const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "neg        ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "dbl        ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sub        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg_inplace(       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "dbl_inplace(       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add_inplace(       uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sub_inplace(       uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "madd_proj_aff ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "madd_aff_proj ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "madd_inplace(         uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "scl_generic( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  , "extern void " ++ prefix ++ "scl_Fr_std ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_Fr_mont( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_big    ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_small  (       uint64_t  kst , const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "scl_naive   ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  , "extern void " ++ prefix ++ "scl_windowed( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  , ""
  ] ++
  (msm_c_header cgparams) ++ 
  (fft_c_header cgparams)
  
--------------------------------------------------------------------------------

hsFFI :: CodeGenParams -> Code
hsFFI (CodeGenParams{..}) = catCode $ 
  [ mkffi "isOnCurve"    $ cfun "is_on_curve"     (CTyp [CArgInProj] CRetBool)
  , mkffi "isInfinity"   $ cfun "is_infinity"     (CTyp [CArgInProj] CRetBool)
  , mkffi "isInSubgroup" $ cfun "is_in_subgroup"  (CTyp [CArgInProj] CRetBool)
    --
  , mkffi "isEqual"     $ cfun "is_equal"         (CTyp [CArgInProj, CArgInProj ] CRetBool)
  , mkffi "isSame"      $ cfun "is_same"          (CTyp [CArgInProj, CArgInProj ] CRetBool)
    --
  , mkffi "normalize"   $ cfun "normalize"        (CTyp [CArgInProj, CArgOutProj ] CRetVoid)
    --
  , mkffi "fromAffine"  $ cfun "from_affine"      (CTyp [CArgInAffine , CArgOutProj   ] CRetVoid)
  , mkffi "toAffine"    $ cfun "to_affine"        (CTyp [CArgInProj   , CArgOutAffine ] CRetVoid)
  , mkffi "batchFromAffine"  $ cfun "batch_from_affine"      (CTyp [CArgCount, CArgInAffineArray , CArgOutProjArray   ] CRetVoid)
  , mkffi "batchToAffine"    $ cfun "batch_to_affine"        (CTyp [CArgCount, CArgInProjArray   , CArgOutAffineArray ] CRetVoid)
    --
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInProj              , CArgOutProj ] CRetVoid)
  , mkffi "dbl"         $ cfun "dbl"              (CTyp [CArgInProj              , CArgOutProj ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInProj , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInProj , CArgInProj , CArgOutProj ] CRetVoid)
    --
  , mkffi "madd"        $ cfun "madd_proj_aff"    (CTyp [CArgInProj , CArgInAffine , CArgOutProj ] CRetVoid)
    --
  , mkffi "sclFr"          $ cfun "scl_Fr_mont"   (CTyp [CArgInScalarR , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sclBigNonNeg"   $ cfun "scl_big"       (CTyp [CArgInBigIntP , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sclSmallNonNeg" $ cfun "scl_small"     (CTyp [CArgInt       , CArgInProj , CArgOutProj ] CRetVoid)
   --
--  -- FOR DEBUGGING ONLY
--  , mkffi "scaleByA"  $ cfun "scale_by_A"  (CTyp [CArgInScalarP , CArgOutScalarP ] CRetVoid)
--  , mkffi "scaleByB"  $ cfun "scale_by_B"  (CTyp [CArgInScalarP , CArgOutScalarP ] CRetVoid)
--  , mkffi "scaleBy3B" $ cfun "scale_by_3B" (CTyp [CArgInScalarP , CArgOutScalarP ] CRetVoid)
  ]
  where
    -- cfun_ cname = CFun (bigint_   ++ cname)
    -- cfun' cname = CFun (stdPrefix ++ cname)
    cfun  cname = CFun (prefix    ++ cname)
    mkffi = curveFfiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeName
      , hsTyCon  = "Mk" ++ typeName
      , hsTyNameProj   = hsModule hs_path_proj   ++ "." ++         typeName
      , hsTyConProj    = hsModule hs_path_proj   ++ "." ++ "Mk" ++ typeName
      , hsTyNameAffine = hsModule hs_path_affine ++ "." ++         typeName
      , hsTyConAffine  = hsModule hs_path_affine ++ "." ++ "Mk" ++ typeName
      , hsNLimbsP = nlimbs_p
      , hsNLimbsR = nlimbs_r
      }

--------------------------------------------------------------------------------

hsBegin :: XCurve -> CodeGenParams -> Code
hsBegin xcurve cgparams@(CodeGenParams{..}) =
  [ "-- | " ++ full_curvename xcurve ++ " curve, projective coordinates, Montgomery field representation"
  , ""
  , "-- NOTE 1: This module is intented to be imported qualified"
  , "-- NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies, PatternSynonyms #-}"
  , "module " ++ hsModule hs_path_proj
  , "  ( " ++ typeName ++ "(..)"
  ] ++ 
  hsExportParams xcurve ++
  [ "    -- * Curve points"
  , "  , coords , mkPoint , mkPointMaybe , unsafeMkPoint"
  , "    -- * Conversion to\\/from affine"
  , "  , fromAffine , toAffine"
  , "  , batchFromAffine , batchToAffine"
  , "  , normalize"
  , "    -- * Predicates"
  , "  , isEqual , isSame"
  , "  , isOnCurve , isInfinity , isInSubgroup"
  , "    -- * Addition and doubling"
  , "  , neg , add , madd, dbl , sub"
  , "    -- * Scaling"
  , "  , sclFr , sclBig , sclSmall"
  , "    -- * Random"
  , "  , rnd" ++ typeName ++ " , rnd" ++ typeName ++ "_naive"
  , "    -- * Multi-scalar multiplication"
  , "  , msm , msmStd , msmProj"
  , "  , msmBinary , binaryMultiMSM , binaryMultiMSM_"
  , "    -- * Fast-Fourier transform"
  , "  , forwardFFT , inverseFFT"
  , "    -- * Sage"
  , "  , sageSetup , printSageSetup"
  , "  )"  
  , "  where"  
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "import Prelude hiding (div)"
  , "-- import GHC.Real hiding (div,infinity)"
  , ""
  , "import Data.Bits"
  , "import Data.Word"
  , ""
  , "import Foreign.C"
  , "import Foreign.Ptr"
  , "import Foreign.Marshal"
  , "import Foreign.ForeignPtr"
  , ""
  , "import System.IO.Unsafe"
  , ""
  ] ++
  (case xcurve of
    Left _ ->
      [ "import " ++ hsModule hs_path_p ++ " ( Fp(..) )"
      , "import " ++ hsModule hs_path_r ++ " ( Fr(..) )"
      , "import qualified " ++ hsModule hs_path_p ++ " as Fp"
      , "import qualified " ++ hsModule hs_path_p ++ " as Base"
      , "import qualified " ++ hsModule hs_path_r ++ " as Fr"
      , "import qualified " ++ hsModule hs_path_r_std 
      , "import qualified " ++ hsModule hs_path_big_p ++ " as BigP"
      ]
    Right _ -> 
      [ "import " ++ hsModule hs_path_p   ++ " ( Fp(..)  )"
      , "import " ++ hsModule hs_path_fp2 ++ " ( Fp2(..) )"
      , "import " ++ hsModule hs_path_r   ++ " ( Fr(..)  )"
      , "import qualified " ++ hsModule hs_path_p   ++ " as Fp"
      , "import qualified " ++ hsModule hs_path_fp2 ++ " as Fp2"
      , "import qualified " ++ hsModule hs_path_fp2 ++ " as Base"
      , "import qualified " ++ hsModule hs_path_r   ++ " as Fr"
      , "import qualified " ++ hsModule hs_path_r_std 
      , "import qualified " ++ hsModule hs_path_big_p ++ " as BigP"
      ]
  ) ++
  [ ""
  , "import {-# SOURCE #-} qualified " ++ hsModule hs_path_affine  -- ++ " as Affine" 
  , ""
  , "import           ZK.Algebra.Class.Flat ( FlatArray(..) )"
  , "import qualified ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as F"
  , "import qualified ZK.Algebra.Class.Curve as C"
  , "import qualified ZK.Algebra.Class.Misc  as M"
  , "import           ZK.Algebra.Class.FFT"
  , ""
  , "import ZK.Algebra.Helpers"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  ] ++ 
  hsBegin_xcurve True xcurve cgparams ++
  [ ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- | An elliptic curve point, in projective coordinates"
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "-- | Note: this throws an exception if the point is not on the curve"
  , "mkPoint :: (Base, Base, Base) -> " ++ typeName
  , "mkPoint xyz = case mkPointMaybe xyz of"
  , "  Just pt -> pt"
  , "  Nothing -> error \"mkPoint: point is not on the curve\""
  , ""
  , "mkPointMaybe :: (Base, Base, Base) -> Maybe " ++ typeName
  , "mkPointMaybe xyz = let pt = unsafeMkPoint xyz in"
  , "  case isOnCurve pt of { True -> Just pt ; False -> Nothing }"
  , ""
  , "-- | The point at infinity"
  , "infinity :: " ++ typeName
  , "infinity = unsafeMkPoint (Base.zero, Base.one, Base.zero)"
  , ""
  , "{-# NOINLINE unsafeMkPoint #-}"
  , "unsafeMkPoint :: (Base, Base, Base) -> " ++ typeName
  , "unsafeMkPoint (MkBase fptr1 , MkBase fptr2 , MkBase fptr3) = unsafePerformIO $ do"
  , "  fptr4 <- mallocForeignPtrArray " ++ show (3*nlimbs_p)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        withForeignPtr fptr4 $ \\ptr4 -> do"
  , "          copyBytes (        ptr4 " ++                      "  ) ptr1 " ++ show (8*nlimbs_p)
  , "          copyBytes (plusPtr ptr4 " ++ show (  8*nlimbs_p) ++ ") ptr2 " ++ show (8*nlimbs_p)
  , "          copyBytes (plusPtr ptr4 " ++ show (2*8*nlimbs_p) ++ ") ptr3 " ++ show (8*nlimbs_p)
  , "  return (Mk" ++ typeName ++ " fptr4)"
  , ""
  , "{-# NOINLINE coords #-}"
  , "coords :: " ++ typeName ++ " -> (Base, Base, Base)"
  , "coords (Mk" ++ typeName ++ " fptr4) = unsafePerformIO $ do"
  , "  fptr1 <- mallocForeignPtrArray " ++ show (nlimbs_p)
  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs_p)
  , "  fptr3 <- mallocForeignPtrArray " ++ show (nlimbs_p)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        withForeignPtr fptr4 $ \\ptr4 -> do"
  , "          copyBytes ptr1 (        ptr4 " ++                      "  ) " ++ show (8*nlimbs_p)
  , "          copyBytes ptr2 (plusPtr ptr4 " ++ show (  8*nlimbs_p) ++ ") " ++ show (8*nlimbs_p)
  , "          copyBytes ptr3 (plusPtr ptr4 " ++ show (2*8*nlimbs_p) ++ ") " ++ show (8*nlimbs_p)
  , "  return (MkBase fptr1, MkBase fptr2, MkBase fptr3)"
  , ""
  , "-- | Returns a uniformly random element /in the subgroup " ++ typeName ++ "/."
  , "-- Note: this is slow, because it uses exponentiation."
  , "rnd" ++ typeName ++ "_naive :: IO " ++ typeName
  , "rnd" ++ typeName ++ "_naive = do"
  , "  k <- Fr.rnd :: IO Fr"
  , "  return (sclFr k gen" ++ typeName ++ ")"
  , "" 
  , "-- | Returns a uniformly random element /in the subgroup " ++ typeName ++ "/."
  , "rnd" ++ typeName ++ " :: IO " ++ typeName
  , "rnd" ++ typeName ++ " = rnd" ++ typeName ++ "_naive"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "instance C.StrictEq " ++ typeName ++ " where"
  , "  (===) = isSame"
  , ""
  , "instance Eq " ++ typeName ++ " where"
  , "  (==) = isEqual"
  , "  -- p == q  =  coords (normalize p) == coords (normalize q)"
  , ""
  , "instance Show " ++ typeName ++ " where"
  , "  show pt = case coords pt of"
  , "     (x,y,z) -> \"[ \" ++ show x ++ \" : \" ++ show y ++ \" : \" ++ show z ++ \" ]\""
  , ""
  , "instance L.Flat " ++ typeName ++ " where"
  , "  sizeInBytes  _pxy = " ++ show (8*3*nlimbs_p)
  , "  sizeInQWords _pxy = " ++ show (  3*nlimbs_p)
  , "  withFlat (Mk" ++ typeName ++ " fptr) = withForeignPtr fptr"
  , "  makeFlat = L.makeFlatGeneric Mk" ++ typeName ++ " " ++ show (3*nlimbs_p)
  , ""
  , "instance M.Rnd " ++ typeName ++ " where"
  , "  rndIO = rnd" ++ typeName
  , ""
  , "instance C.Group " ++ typeName ++ " where"
  , "  grpName _    = \"" ++ x_groupname xcurve ++ "\""
  , "  grpIsUnit    = " ++ hsModule hs_path_proj ++ ".isInfinity"
  , "  grpUnit      = " ++ hsModule hs_path_proj ++ ".infinity"
  , "  grpNormalize = normalize"
  , "  grpNeg       = neg"
  , "  grpDbl       = dbl"
  , "  grpAdd       = add"
  , "  grpSub       = sub"
  , "  grpScale_    = sclSmall"
  , "  grpScale     = sclBig"
  , ""
  , "instance C.Curve " ++ typeName ++ " where"
  , "  curveNamePxy _ = \"" ++ full_curvename xcurve ++ "\""
  , "  type BaseField   " ++ typeName ++ " = Base"
  , "  type ScalarField " ++ typeName ++ " = Fr"
  , "  isOnCurve   = " ++ hsModule hs_path_proj ++ ".isOnCurve"
  , "  isInfinity  = " ++ hsModule hs_path_proj ++ ".isInfinity"
  , "  infinity    = " ++ hsModule hs_path_proj ++ ".infinity"
  , "  curveSubgroupGen = " ++ hsModule hs_path_proj ++ ".gen" ++ typeName
  , "  scalarMul   = " ++ hsModule hs_path_proj ++ ".sclFr"
  , "  msm         = " ++ hsModule hs_path_proj ++ ".msmProj"
--  , "  msmStd      = " ++ hsModule hs_path_proj ++ ".msmStd"
  , "  curveFFT    = " ++ hsModule hs_path_proj ++ ".forwardFFT"
  , "  curveIFFT   = " ++ hsModule hs_path_proj ++ ".inverseFFT"
  , ""
  , "instance C.ProjCurve " ++ typeName ++ " where"
  , "  type AffinePoint " ++ typeName ++ " = " ++ hsModule hs_path_affine ++ "." ++ typeName
  , "  fromAffine = " ++ hsModule hs_path_proj ++ ".fromAffine"
  , "  toAffine   = " ++ hsModule hs_path_proj ++ ".toAffine"
  , "  batchFromAffine = " ++ hsModule hs_path_proj ++ ".batchFromAffine"
  , "  batchToAffine   = " ++ hsModule hs_path_proj ++ ".batchToAffine"
  , "  coords3    = " ++ hsModule hs_path_proj ++ ".coords"
  , "  mkPoint3   = " ++ hsModule hs_path_proj ++ ".mkPoint"
  , "  mixedAdd   = " ++ hsModule hs_path_proj ++ ".madd"
  , "  affMSM     = " ++ hsModule hs_path_proj ++ ".msm"
  , "  binaryMSM       = " ++ hsModule hs_path_proj ++ ".msmBinary"
  , "  binaryMultiMSM  = " ++ hsModule hs_path_proj ++ ".binaryMultiMSM"
  , "  "
  , "--------------------------------------------------------------------------------"
  , ""
  , "sclSmall :: Int -> " ++ typeName ++ " -> " ++ typeName
  , "sclSmall k pt"
  , "  | k == 0    = infinity"
  , "  | k < 0     = neg $ sclSmallNonNeg (negate k) pt"
  , "  | otherwise =       sclSmallNonNeg (       k) pt"
  , ""
  , "sclBig :: Integer -> " ++ typeName ++ " -> " ++ typeName
  , "sclBig k pt"
  , "  | k == 0    = infinity"
  , "  | k < 0     = neg $ sclBigNonNeg (fromInteger $ negate k) pt"
  , "  | otherwise =       sclBigNonNeg (fromInteger $        k) pt"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "msmProj :: FlatArray Fr -> FlatArray " ++ typeName ++ " -> " ++ typeName
  , "msmProj cs gs = msm cs (batchToAffine gs)"
  , ""
  , "--------------------------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------

c_begin :: XCurve -> CodeGenParams -> Code
c_begin xcurve cgparams@(CodeGenParams{..}) =
  [ "// elliptic curve " ++ show (full_curvename xcurve) ++ " in projective coordinates, Montgomery field representation"
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <string.h>"
  , "#include <stdlib.h>"
  , "#include <stdint.h>"
  , "#include <assert.h>"
  , "#include <math.h>         // used only for log() and log2()"
  , ""
  , "#include \"" ++ pathBaseName c_path_proj   ++ ".h\""
  , "#include \"" ++ pathBaseName c_path_affine ++ ".h\""
  , "#include \"" ++ c_basename_p  ++ ".h\""
  , "#include \"" ++ c_basename_r  ++ ".h\""
  , ""
  , "#define NLIMBS_P " ++ show nlimbs_p
  , "#define NLIMBS_R " ++ show nlimbs_r
  , ""
  , "#define X1 (src1)"
  , "#define Y1 (src1 + " ++ show (  nlimbs_p) ++ ")"
  , "#define Z1 (src1 + " ++ show (2*nlimbs_p) ++ ")"
  , ""
  , "#define X2 (src2)"
  , "#define Y2 (src2 + " ++ show (  nlimbs_p) ++ ")"
  , "#define Z2 (src2 + " ++ show (2*nlimbs_p) ++ ")"
  , ""
  , "#define X3 (tgt)"
  , "#define Y3 (tgt + " ++ show (  nlimbs_p) ++ ")"
  , "#define Z3 (tgt + " ++ show (2*nlimbs_p) ++ ")"
  , ""
  ] ++
  (case xcurve of 
    Left  curve1  -> c_begin_curve1 True curve1  cgparams
    Right curve12 -> c_begin_curve2 True curve12 cgparams
  ) ++
  [ ""
  , "//------------------------------------------------------------------------------"
  ] 

--------------------------------------------------------------------------------

scale_by_AB3B :: XCurve -> CodeGenParams -> Code
scale_by_AB3B xcurve params = case xcurve of
  Left curve1 -> concat
    [ g1_scale_by_A  curve1 params
    , g1_scale_by_B  curve1 params
    , g1_scale_by_3B curve1 params
    ]
  Right curve12 -> concat
    [ g2_scale_by_A  curve12 params
    , g2_scale_by_B  curve12 params
    , g2_scale_by_3B curve12 params
    ]

--------------------------------------------------------------------------------

g2_scale_by_A :: Curve12 -> CodeGenParams -> Code
g2_scale_by_A (Curve12 (Curve1{..}) (Curve2{..})) (CodeGenParams{..}) = case g2_curveA of
  (0,0) -> [ "// scale an Fp2 field element by A = " ++ show g2_curveA
           , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           ]

  _ ->     [ "// scale an Fp2 field element by A = " ++ show g2_curveA
           , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_A, src, tgt );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_A );"
           , "}"
           ]

g2_scale_by_B :: Curve12 -> CodeGenParams -> Code
g2_scale_by_B (Curve12 (Curve1{..}) (Curve2{..})) (CodeGenParams{..}) = case g2_curveB of
  (0,0) -> [ "// scale an Fp2 field element by B = " ++ show g2_curveB
           , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           ]

  _ ->     [ "// scale an Fp field element by B = " ++ show g2_curveB
           , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_B, src, tgt );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_B );"
           , "}"
           ]

g2_scale_by_3B :: Curve12 -> CodeGenParams -> Code
g2_scale_by_3B (Curve12 (Curve1{..}) (Curve2{..})) (CodeGenParams{..}) = case g2_curveB of
  (0,0) -> [ "// scale an Fp2 field element by 3B = 3*" ++ show g2_curveB
           , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
           , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
           , "}"
           ]

  _ ->     [ "// scale an Fp field element by 3B = 3*" ++ show g2_curveB
           , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_3B, src, tgt );"
           , "}"
           , ""
           , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
           , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_3B );"
           , "}"
           ]

--------------------------------------------------------------------------------

g1_scale_by_A ::  Curve1 -> CodeGenParams -> Code
g1_scale_by_A (Curve1{..}) (CodeGenParams{..}) = case curveA of

  0 -> [ "// scale a field element by A = " ++ show curveA
       , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       ]

  1 -> [ "// scale a field element by A = " ++ show curveA
       , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
       , "  if (tgt != src) { memcpy( tgt, src, " ++ show (8*nlimbs_p) ++ " ); }"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  // no-op"
       , "}"
       ]

  2 -> [ "// scale a field element by A = " ++ show curveA
       , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "add( src, src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "add_inplace( tgt, tgt );"
       , "}"
       ]

  _ -> [ "// scale a field element by A = " ++ show curveA
       , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_A, src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_A );"
       , "}"
       ]

----------------------------------------

g1_scale_by_B ::  Curve1 -> CodeGenParams -> Code
g1_scale_by_B (Curve1{..}) (CodeGenParams{..}) = case curveB of

  0 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       ]

  1 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  if (tgt != src) { memcpy( tgt, src, " ++ show (8*nlimbs_p) ++ " ); }"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  // no-op"
       , "}"
       ]

  2 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "add( src, src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "add_inplace( tgt, tgt );"
       , "}"
       ]

  3 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
       , "  " ++ prefix_p ++ "add( src, src, tmp );"
       , "  " ++ prefix_p ++ "add( src, tmp, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
       , "  " ++ prefix_p ++ "add( tgt, tgt, tmp );"
       , "  " ++ prefix_p ++ "add_inplace( tgt, tmp );"
       , "}"
       ]

  4 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
       , "  " ++ prefix_p ++ "add( src, src, tmp );"
       , "  " ++ prefix_p ++ "add( tmp, tmp, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
       , "  " ++ prefix_p ++ "add( tgt, tgt, tmp );"
       , "  " ++ prefix_p ++ "add( tmp, tmp, tgt );"
       , "}"
       ]

  _ -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_B, src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_B );"
       , "}"
       ]

----------------------------------------

g1_scale_by_3B ::  Curve1 -> CodeGenParams -> Code
g1_scale_by_3B (Curve1{..}) (CodeGenParams{..}) = case curveB of

  0 -> [ "// scale a field element by (3*B) = " ++ show (3*curveB)
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
       , "}"
       ]

  3 -> [ "// scale a field element by (3*B) = " ++ show (3*curveB)
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  uint64_t tmp[NLIMBS_P];"
       , "  " ++ prefix_p ++ "add( src, src, tmp );       // 2*B"
       , "  " ++ prefix_p ++ "add_inplace( tmp, tmp );    // 4*B"
       , "  " ++ prefix_p ++ "add_inplace( tmp, tmp );    // 8*B"
       , "  " ++ prefix_p ++ "add( src, tmp, tgt );       // 9*B"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix ++ "scale_by_3B( tgt , tgt );"
       , "}"
       ]

  4  ->[ "// scale a field element by (3*B) = " ++ show (3*curveB)
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  uint64_t tmp [NLIMBS_P];"
       , "  uint64_t tmp2[NLIMBS_P];"
       , "  " ++ prefix_p ++ "add( src, src, tmp );       // 2*B"
       , "  " ++ prefix_p ++ "add_inplace( tmp, tmp );    // 4*B"
       , "  " ++ prefix_p ++ "add( tmp, tmp, tmp2);       // 8*B"
       , "  " ++ prefix_p ++ "add( tmp, tmp2, tgt );      // 12*B"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix ++ "scale_by_3B( tgt , tgt );"
       , "}"
       ]

  _ -> [ "// scale a field element by 3B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul( " ++ prefix ++ "const_3B, src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ prefix ++ "const_3B );"
       , "}"
       ]

--------------------------------------------------------------------------------

normalize :: CodeGenParams -> Code
normalize (CodeGenParams{..}) =
  [ "void " ++ prefix ++ "normalize( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t zinv[" ++ show nlimbs_p ++ "];"
  , "  if (" ++ prefix_p ++ "is_zero( Z1 ) ) {"
  , "    // Z == 0, it must be the point at infinity"
  , "    memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
  , "    " ++ prefix_p ++ "set_one( Y3 );"
  , "  }"
  , "  else {"
  , "    if (" ++ prefix_p ++ "is_one( Z1 )) {"
  , "      // already normalized"
  , "      if (tgt != src1) { memcpy( tgt, src1, " ++ show (8*3*nlimbs_p) ++ " ); }"
  , "    }"
  , "    else {"
  , "      " ++ prefix_p ++ "inv( Z1, zinv );"
  , "      " ++ prefix_p ++ "mul( X1, zinv, X3 );"
  , "      " ++ prefix_p ++ "mul( Y1, zinv, Y3 );"
  , "      " ++ prefix_p ++ "set_one( Z3 );"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "normalize_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "normalize( tgt, tgt );"
  , "}"
  , ""
  , "// checks whether the underlying representation (projective coordinates) are the same"
  , "uint8_t " ++ prefix ++ "is_same( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  return ( " ++ prefix_p ++ "is_equal( X1, X2 ) &&"
  , "           " ++ prefix_p ++ "is_equal( Y1, Y2 ) &&"
  , "           " ++ prefix_p ++ "is_equal( Z1, Z2 ) );"
  , "}"
  , ""
  , "// checks whether two curve points are equal"
  , "uint8_t " ++ prefix ++ "is_equal( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  uint64_t tmp1[" ++ show(3*nlimbs_p) ++ "];"
  , "  uint64_t tmp2[" ++ show(3*nlimbs_p) ++ "];"
  , "  " ++ prefix ++ "normalize( src1, tmp1 );"
  , "  " ++ prefix ++ "normalize( src2, tmp2 );"
  , "  return " ++ prefix ++ "is_same( tmp1, tmp2 );"
  , "}"
  ]

convertAffine :: CodeGenParams -> Code
convertAffine (CodeGenParams{..}) = 
  [ "// converts from affine coordinates"
  , "void " ++ prefix ++ "from_affine( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  memcpy( tgt, src1, " ++ show (8*2*nlimbs_p) ++ " );"
  , "  if (" ++ prefix_affine ++ "is_infinity( src1 )) {"
  , "    " ++ prefix ++ "set_infinity( tgt );"
  , "  }"
  , "  else {"
  , "    " ++ prefix_p ++ "set_one( Z3 );"
  , "  }"
  , "}"
  , ""
  , "// converts to affine coordinates"
  , "// remark: the point at infinity will result in the special string `0xffff...ffff`"
  , "void " ++ prefix ++ "to_affine( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  if (" ++ prefix_p ++ "is_zero( Z1 )) {"
  , "    // in the affine coordinate system, the point at infinity is represented by a hack"
  , "    // consisting all 0xff bytes (note that that's an invalid value for prime fields)"
  , "    memset( tgt, 0xff, " ++ show (8*2*nlimbs_p) ++ " );"
  , "  }"
  , "  else {"
  , "    uint64_t zinv[" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "inv( Z1, zinv );"
  , "    " ++ prefix_p ++ "mul( X1, zinv, X3 );"
  , "    " ++ prefix_p ++ "mul( Y1, zinv, Y3 );"
  , "  }"
  , "}"
  , ""
  , "// converts N points from affine coordinates"
  , "void " ++ prefix ++ "batch_from_affine( int N, const uint64_t *src , uint64_t *tgt ) {"
  , "  const uint64_t *p = src;"
  , "  uint64_t *q = tgt;"
  , "  for(int i=0; i<N; i++) {"
  , "    " ++ prefix ++ "from_affine(p,q);"
  , "    p += 2*NLIMBS_P;"
  , "    q += 3*NLIMBS_P;"
  , "  }"
  , "}"
  , ""
  , "// converts N points to affine coordinates"
  , "void " ++ prefix ++ "batch_to_affine( int N, const uint64_t *src , uint64_t *tgt ) {"
  , "  const uint64_t *p = src;"
  , "  uint64_t *q = tgt;"
  , "  for(int i=0; i<N; i++) {"
  , "    " ++ prefix ++ "to_affine(p,q);"
  , "    p += 3*NLIMBS_P;"
  , "    q += 2*NLIMBS_P;"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  if (tgt != src1) { memcpy( tgt, src1, " ++ show (8*3*nlimbs_p) ++ " ); }"
  , "}"  
  ]

isOnCurve :: XCurve -> CodeGenParams -> Code
isOnCurve xcurve (CodeGenParams{..}) = 
  [ "uint8_t " ++ prefix ++ "is_infinity ( const uint64_t *src1 ) {"
  , "  return ( ( " ++ prefix_p ++ "is_zero( Z1 )) &&"
  , "           (!" ++ prefix_p ++ "is_zero( Y1 )) &&"
  , "           ( " ++ prefix_p ++ "is_zero( X1 )) );"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_infinity ( uint64_t *tgt ) {"
  , "  " ++ prefix_p ++ "set_zero( X3 );"
  , "  " ++ prefix_p ++ "set_one ( Y3 );"
  , "  " ++ prefix_p ++ "set_zero( Z3 );"
  , "}"
  , ""
  , "// checks the curve equation"
  , "//   y^2*z == x^3 + A*x*z^2 + B*z^3"
  , "uint8_t " ++ prefix ++ "is_on_curve ( const uint64_t *src1 ) {"
  , "  uint64_t  ZZ[" ++ show nlimbs_p ++ "];"
  , "  uint64_t acc[" ++ show nlimbs_p ++ "];"
  , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
  , "  " ++ prefix_p ++ "sqr( Y1, acc );             // Y^2"
  , "  " ++ prefix_p ++ "mul_inplace( acc, Z1 );     // Y^2*Z"
  , "  " ++ prefix_p ++ "neg_inplace( acc );         // -Y^2*Z"
  , "  " ++ prefix_p ++ "sqr( X1, tmp );             // X^2"
  , "  " ++ prefix_p ++ "mul_inplace( tmp, X1 );     // X^3"
  , "  " ++ prefix_p ++ "add_inplace( acc, tmp );    // - Y^2*Z + X^3"
  , "  " ++ prefix_p ++ "sqr( Z1, ZZ );              // Z^2"
  ] ++ 
  (if isCurveAZero xcurve then [] else 
    [ "  " ++ prefix_p ++ "mul( X1, ZZ, tmp );          // X*Z^2"
    , "  " ++ prefix   ++ "scale_by_A_inplace( tmp );   // A*X*Z^2"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2"
    ]
  ) ++ 
  (if isCurveBZero xcurve then [] else 
    [ "  " ++ prefix_p ++ "mul( Z1, ZZ, tmp );          // Z^3"
    , "  " ++ prefix   ++ "scale_by_B_inplace( tmp );   // B*Z^3"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2 + B*Z^3"
    ]
  ) ++
  [ "  return (" ++ prefix_p ++ "is_zero( acc ) &&"
  , "           ( (!" ++ prefix_p ++ "is_zero( Z1 )) || "
  , "             (!" ++ prefix_p ++ "is_zero( Y1 )) ) );"
  , "}"
  , ""
  , "// checks whether the given point is in the subgroup " ++ typeName
  , "uint8_t " ++ prefix ++ "is_in_subgroup ( const uint64_t *src1 ) {"
  , "  uint64_t tmp[" ++ show (3*nlimbs_p) ++ "];"
  , "  if (!" ++ prefix ++ "is_on_curve(src1)) {"
  , "    return 0;"
  , "  }"
  , "  else {"
  , "    " ++ prefix ++ "scl_Fr_std( " ++ prefix ++ "cofactor , src1 , tmp );"
  , "    return " ++ prefix ++ "is_infinity( tmp );"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

negCurve :: CodeGenParams -> Code
negCurve (CodeGenParams{..}) =
  [ "// negates an elliptic curve point" 
  , "void " ++ prefix ++ "neg( const uint64_t *src, uint64_t *tgt ) {"
  , "  if (tgt != src) { memcpy( tgt, src, " ++ show (8*3*nlimbs_p) ++ " ); }"
  , "  " ++ prefix_p ++ "neg_inplace( Y3 );"
  , "}"
  , ""
  , "// negates an elliptic curve point" 
  , "void " ++ prefix ++ "neg_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix_p ++ "neg_inplace( Y3 );"
  , "}"
  ]

--------------------------------------------------------------------------------

{-
  formula from <https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#doubling-dbl-2007-bl.
    XX = X1^2
    ZZ = Z1^2
    w = a*ZZ+3*XX
    s = 2*Y1*Z1
    ss = s^2
    sss = s*ss
    R = Y1*s
    RR = R2
    B = (X1+R)^2-XX-RR
    h = w^2-2*B
    X3 = h*s
    Y3 = w*(B-h)-2*RR
    Z3 = sss
-}
dblCurve :: XCurve -> CodeGenParams -> Code
dblCurve xcurve (CodeGenParams{..}) =
  [ "// doubles an elliptic curve point" 
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#doubling-dbl-2007-bl"
  , "void " ++ prefix ++ "dbl( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t  XX[" ++ show nlimbs_p ++ "];"
  , "  uint64_t  ZZ[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   w[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   s[" ++ show nlimbs_p ++ "];"
  , "  uint64_t  ss[" ++ show nlimbs_p ++ "];"
  , "  uint64_t sss[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   R[" ++ show nlimbs_p ++ "];"
  , "  uint64_t  RR[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   B[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   h[" ++ show nlimbs_p ++ "];"
  , "  " ++ prefix_p ++ "sqr( X1 , XX );         // XX = X1^2"
  , "  " ++ prefix_p ++ "sqr( Z1 , ZZ );         // ZZ = Z1^2"
  , "  " ++ prefix_p ++ "add( XX, XX, w );       // w  = 2*XX"
  , "  " ++ prefix_p ++ "add_inplace( w, XX );   // w  = 3*XX"   
  ] ++ (if isCurveAZero xcurve then [] else 
    [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
    , "  " ++ prefix   ++ "scale_by_A ( ZZ , tmp );   // tmp =  a*ZZ"
    , "  " ++ prefix_p ++ "add_inplace ( w , tmp );   // w = a*ZZ + 3*XX"
    ]) ++
  [ "  " ++ prefix_p ++ "mul( Y1 , Z1 , s );         // s   = Y1*Z1"
  , "  " ++ prefix_p ++ "add_inplace( s, s );        // s   = 2*Y1*Z1"
  , "  " ++ prefix_p ++ "sqr( s , ss );              // ss  = s^2"
  , "  " ++ prefix_p ++ "mul( Y1 , s , R);           // R   = Y1*s"
  , "  " ++ prefix_p ++ "sqr( R , RR );              // RR  = R^2"
  , "  " ++ prefix_p ++ "add( X1 , R , B);           // B   = (X1+R)"
  , "  " ++ prefix_p ++ "sqr_inplace( B );           // B   = (X1+R)^2"
  , "  " ++ prefix_p ++ "sub_inplace( B , XX );      // B   = (X1+R)^2 - XX"
  , "  " ++ prefix_p ++ "sub_inplace( B , RR );      // B   = (X1+R)^2 - XX - RR"
  , "  " ++ prefix_p ++ "sqr( w , h );               // h   = w^2"
  , "  " ++ prefix_p ++ "sub_inplace( h , B );       // h   = w^2 - B"
  , "  " ++ prefix_p ++ "sub_inplace( h , B );       // h   = w^2 - 2*B"
  , "  " ++ prefix_p ++ "mul( s , ss , Z3 );         // Z3  = s^3"
  , "  " ++ prefix_p ++ "mul( h , s , X3 );          // X3  = h*s"
  , "  " ++ prefix_p ++ "sub( B , h , Y3 );          // Y3  = B-h"
  , "  " ++ prefix_p ++ "mul_inplace( Y3 , w  );     // Y3  = w*(B-h)"
  , "  " ++ prefix_p ++ "sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - RR"
  , "  " ++ prefix_p ++ "sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - 2*RR"
  , "}"
  , ""
  , "// doubles an elliptic curve point" 
  , "void " ++ prefix ++ "dbl_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "dbl( tgt , tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

addCurve :: CodeGenParams -> Code
addCurve (CodeGenParams{..}) =
  [ "// adds two elliptic curve points" 
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb"
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t t0[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t3[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t4[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t5[" ++ show nlimbs_p ++ "];"  
  , "  " ++ prefix_p ++ "mul( X1, X2, t0 );              // t0 = X1*X2"
  , "  " ++ prefix_p ++ "mul( Y1, Y2, t1 );              // t1 = Y1*Y2"
  , "  " ++ prefix_p ++ "mul( Z1, Z2, t2 );              // t2 = Z1*Z2"
  , "  " ++ prefix_p ++ "add( X1, Y1, t3 );              // t3 = X1+Y1"
  , "  " ++ prefix_p ++ "add( X2, Y2, t4 );              // t4 = X2+Y2"
  , "  " ++ prefix_p ++ "mul_inplace( t3, t4 );          // t3 = t3*t4"
  , "  " ++ prefix_p ++ "add( t0, t1, t4 );              // t4 = t0+t1"
  , "  " ++ prefix_p ++ "sub_inplace( t3 , t4 );         // t3 = t3-t4"
  , "  " ++ prefix_p ++ "add( X1, Z1, t4 );              // t4 = X1+Z1"
  , "  " ++ prefix_p ++ "add( X2, Z2, t5 );              // t5 = X2+Z2"
  , "  " ++ prefix_p ++ "mul_inplace( t4, t5 );          // t4 = t4*t5"
  , "  " ++ prefix_p ++ "add( t0, t2, t5 );              // t5 = t0+t2"
  , "  " ++ prefix_p ++ "sub_inplace( t4, t5 );          // t4 = t4-t5"
  , "  " ++ prefix_p ++ "add( Y1, Z1, t5 );              // t5 = Y1+Z1"
  , "  " ++ prefix_p ++ "add( Y2, Z2, X3 );              // X3 = Y2+Z2"
  , "  " ++ prefix_p ++ "mul_inplace( t5, X3 );          // t5 = t5*X3"
  , "  " ++ prefix_p ++ "add( t1, t2, X3 );              // X3 = t1+t2"
  , "  " ++ prefix_p ++ "sub_inplace( t5, X3 );          // t5 = t5-X3"
  , "  " ++ prefix   ++ "scale_by_A ( t4, Z3 );          // Z3 = a*t4 "
  , "  " ++ prefix   ++ "scale_by_3B( t2, X3 );          // X3 = b3*t2"
  , "  " ++ prefix_p ++ "add_inplace( Z3, X3 );          // Z3 = X3+Z3"
  , "  " ++ prefix_p ++ "sub( t1, Z3, X3 );              // X3 = t1-Z3"
  , "  " ++ prefix_p ++ "add_inplace( Z3, t1 );          // Z3 = t1+Z3"
  , "  " ++ prefix_p ++ "mul( X3, Z3, Y3 );              // Y3 = X3*Z3"
  , "  " ++ prefix_p ++ "add( t0, t0, t1 );              // t1 = t0+t0"
  , "  " ++ prefix_p ++ "add_inplace( t1, t0 );          // t1 = t1+t0"
  , "  " ++ prefix   ++ "scale_by_A_inplace ( t2 );      // t2 = a*t2 "
  , "  " ++ prefix   ++ "scale_by_3B_inplace( t4 );      // t4 = b3*t4"
  , "  " ++ prefix_p ++ "add_inplace( t1, t2 );          // t1 = t1+t2"
  , "  " ++ prefix_p ++ "sub_inplace_reverse( t2, t0 );  // t2 = t0-t2"
  , "  " ++ prefix   ++ "scale_by_A_inplace ( t2 );      // t2 = a*t2 "
  , "  " ++ prefix_p ++ "add_inplace( t4, t2 );          // t4 = t4+t2"
  , "  " ++ prefix_p ++ "mul( t1, t4, t0 );              // t0 = t1*t4"
  , "  " ++ prefix_p ++ "add_inplace( Y3, t0 );          // Y3 = Y3+t0"
  , "  " ++ prefix_p ++ "mul( t4, t5, t0 );              // t0 = t5*t4"
  , "  " ++ prefix_p ++ "mul_inplace( X3, t3 );          // X3 = t3*X3"
  , "  " ++ prefix_p ++ "sub_inplace( X3, t0 );          // X3 = X3-t0"
  , "  " ++ prefix_p ++ "mul( t1, t3, t0 );              // t0 = t3*t1"
  , "  " ++ prefix_p ++ "mul_inplace( Z3, t5 );          // Z3 = t5*Z3"
  , "  " ++ prefix_p ++ "add_inplace( Z3, t0 );          // Z3 = Z3+t0"
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "add( tgt, src2, tgt);"
  , "}"
  ]

addCurveA0 :: CodeGenParams -> Code
addCurveA0 (CodeGenParams{..}) =
  [ "// adds two elliptic curve points, assuming A = 0" 
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb"
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t t0[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t3[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t4[" ++ show nlimbs_p ++ "];"
  , "  uint64_t t5[" ++ show nlimbs_p ++ "];"  
  , "  " ++ prefix_p ++ "mul( X1, X2, t0 );              // t0 = X1*X2"
  , "  " ++ prefix_p ++ "mul( Y1, Y2, t1 );              // t1 = Y1*Y2"
  , "  " ++ prefix_p ++ "mul( Z1, Z2, t2 );              // t2 = Z1*Z2"
  , "  " ++ prefix_p ++ "add( X1, Y1, t3 );              // t3 = X1+Y1"
  , "  " ++ prefix_p ++ "add( X2, Y2, t4 );              // t4 = X2+Y2"
  , "  " ++ prefix_p ++ "mul_inplace( t3, t4 );          // t3 = t3*t4"
  , "  " ++ prefix_p ++ "add( t0, t1, t4 );              // t4 = t0+t1"
  , "  " ++ prefix_p ++ "sub_inplace( t3 , t4 );         // t3 = t3-t4"
  , "  " ++ prefix_p ++ "add( X1, Z1, t4 );              // t4 = X1+Z1"
  , "  " ++ prefix_p ++ "add( X2, Z2, t5 );              // t5 = X2+Z2"
  , "  " ++ prefix_p ++ "mul_inplace( t4, t5 );          // t4 = t4*t5"
  , "  " ++ prefix_p ++ "add( t0, t2, t5 );              // t5 = t0+t2"
  , "  " ++ prefix_p ++ "sub_inplace( t4, t5 );          // t4 = t4-t5"
  , "  " ++ prefix_p ++ "add( Y1, Z1, t5 );              // t5 = Y1+Z1"
  , "  " ++ prefix_p ++ "add( Y2, Z2, X3 );              // X3 = Y2+Z2"
  , "  " ++ prefix_p ++ "mul_inplace( t5, X3 );          // t5 = t5*X3"
  , "  " ++ prefix_p ++ "add( t1, t2, X3 );              // X3 = t1+t2"
  , "  " ++ prefix_p ++ "sub_inplace( t5, X3 );          // t5 = t5-X3"
  , "  " ++ prefix   ++ "scale_by_3B( t2, X3 );          // X3 = b3*t2"
  , "  " ++ prefix_p ++ "copy( X3, Z3 );                 // Z3 = X3"
  , "  " ++ prefix_p ++ "sub( t1, Z3, X3 );              // X3 = t1-Z3"
  , "  " ++ prefix_p ++ "add_inplace( Z3, t1 );          // Z3 = t1+Z3"
  , "  " ++ prefix_p ++ "mul( X3, Z3, Y3 );              // Y3 = X3*Z3"
  , "  " ++ prefix_p ++ "add( t0, t0, t1 );              // t1 = t0+t0"
  , "  " ++ prefix_p ++ "add_inplace( t1, t0 );          // t1 = t1+t0"
  , "  " ++ prefix   ++ "scale_by_3B_inplace( t4 );      // t4 = b3*t4"
  , "  " ++ prefix_p ++ "mul( t1, t4, t0 );              // t0 = t1*t4"
  , "  " ++ prefix_p ++ "add_inplace( Y3, t0 );          // Y3 = Y3+t0"
  , "  " ++ prefix_p ++ "mul( t4, t5, t0 );              // t0 = t5*t4"
  , "  " ++ prefix_p ++ "mul_inplace( X3, t3 );          // X3 = t3*X3"
  , "  " ++ prefix_p ++ "sub_inplace( X3, t0 );          // X3 = X3-t0"
  , "  " ++ prefix_p ++ "mul( t1, t3, t0 );              // t0 = t3*t1"
  , "  " ++ prefix_p ++ "mul_inplace( Z3, t5 );          // Z3 = t5*Z3"
  , "  " ++ prefix_p ++ "add_inplace( Z3, t0 );          // Z3 = Z3+t0"
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "add( tgt, src2, tgt);"
  , "}"
  ]

subCurve :: CodeGenParams -> Code
subCurve (CodeGenParams{..}) =
  [ "void " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t tmp[" ++ show (3*nlimbs_p) ++ "];"
  , "  " ++ prefix ++ "neg( src2, tmp );"
  , "  " ++ prefix ++ "add( src1, tmp, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint64_t tmp[" ++ show (3*nlimbs_p) ++ "];"
  , "  " ++ prefix ++ "neg( src2, tmp );"
  , "  " ++ prefix ++ "add( tgt , tmp, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

{-

https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-madd-1998-cmo

assumption: Z2 == 1

    u = Y2*Z1-Y1
    uu = u2
    v = X2*Z1-X1
    vv = v2
    vvv = v*vv
    R = vv*X1
    A = uu*Z1-vvv-2*R
    X3 = v*A
    Y3 = u*(R-A)-vvv*Y1
    Z3 = vvv*Z1
-}

mixedAddCurve :: CodeGenParams -> Code
mixedAddCurve (CodeGenParams{..}) =
  [ "// adds a projective point (src1) to an affine point (src2)"
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-madd-1998-cmo"
  , "void " ++ prefix ++ "madd_proj_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity( src1 )) {"
  , "    // the formula is not valid for this case"
  , "    " ++ prefix ++ "from_affine( src2 , tgt );"
  , "    return;"
  , "  }"
  , "  if (" ++ prefix_affine ++ "is_infinity( src2 )) {"
  , "    " ++ prefix ++ "copy( src1 , tgt );"
  , "    return;"
  , "  }"
  , "  uint64_t   u[" ++ show nlimbs_p ++ "];"
  , "  uint64_t  uu[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   v[" ++ show nlimbs_p ++ "];"
  , "  uint64_t  vv[" ++ show nlimbs_p ++ "];"
  , "  uint64_t vvv[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   R[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   A[" ++ show nlimbs_p ++ "];"  
  , "  " ++ prefix_p ++ "mul( Y2, Z1, u );              //  u = Y2*Z1     "     
  , "  " ++ prefix_p ++ "sub_inplace( u , Y1 );         //  u = Y2*Z1-Y1  "     
  , "  " ++ prefix_p ++ "mul( X2, Z1, v );              //  v = X2*Z1     "     
  , "  " ++ prefix_p ++ "sub_inplace( v , X1 );         //  v = X2*Z1-X1  "     
  , "  if (" ++ prefix_p ++ "is_zero(u) && " ++ prefix_p ++ "is_zero(v) ) {"
  , "    // it's doubling, the naive result would be (0,0,0)"
  , "    " ++ prefix   ++ "dbl( src1 , tgt );"
  , "    return;"
  , "  }"
  , "  " ++ prefix_p ++ "sqr( u , uu );                 //  uu = u2       "
  , "  " ++ prefix_p ++ "sqr( v, vv );                  //  vv = v2       "
  , "  " ++ prefix_p ++ "mul( v, vv, vvv );             //  vvv = v*vv    "   
  , "  " ++ prefix_p ++ "mul( vv, X1, R );              //  R = vv*X1     "  
  , "  " ++ prefix_p ++ "mul( uu, Z1, A );              //  A = uu*Z1     "          
  , "  " ++ prefix_p ++ "sub_inplace( A, vvv );         //  A = uu*Z1-vvv "          
  , "  " ++ prefix_p ++ "sub_inplace( A, R );           //  A = uu*Z1-vvv-2  "          
  , "  " ++ prefix_p ++ "sub_inplace( A, R );           //  A = uu*Z1-vvv-2*R"          
  , "  " ++ prefix_p ++ "mul( v, A, X3 );               //  X3 = v*A      " 
  , "  " ++ prefix_p ++ "mul( Z1, vvv, Z3 );            //  Z3 = vvv*Z1   "    
  , "  " ++ prefix_p ++ "sub_inplace( R , A );          //  R' =  R-A     "
  , "  " ++ prefix_p ++ "mul_inplace( vvv , Y1 );       //  vvv' = vvv*Y1 "
  , "  " ++ prefix_p ++ "mul( u , R , Y3 );             //  Y3 = u*(R-A)  "
  , "  " ++ prefix_p ++ "sub_inplace( Y3, vvv );        //  Y3 = u*(R-A)-vvv*Y1"            
  , "}"
  , ""
  , "// adds an affine point (src1) to a projective one (src2)"
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb"
  , "void " ++ prefix ++ "madd_aff_proj( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  " ++ prefix ++ "madd_proj_aff( src2, src1, tgt );"
  , "}"
  , ""
  , "// adds to a projective point (tgt) an affine point (src2), in place"
  , "void " ++ prefix ++ "madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "madd_proj_aff( tgt, src2, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

scaleNaive :: CodeGenParams -> Code
scaleNaive (CodeGenParams{..}) =
  [ "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in " ++ typeName ++ ", and `expo` is a (non-negative) bigint"
  , "// naive algorithm"
  , "void " ++ prefix ++ "scl_naive(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {"
  , ""
  , "  uint64_t dbl[3*NLIMBS_P];"
  , "  " ++ prefix ++ "copy( grp, dbl );              // dbl := grp"
  , "  " ++ prefix ++ "set_infinity( tgt );           // tgt := infinity"
  , ""
  , "  int s = expo_len - 1;"
  , "  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers"
  , ""
  , "  for(int i=0; i<=s; i++) {"
  , "    uint64_t e = expo[i];"
  , "    for(int j=0; j<64; j++) {"
  , "      if (e & 1) { "
  , "        " ++ prefix ++ "add( tgt, dbl, tgt ); "
  , "      }"
  , "      " ++ prefix ++ "dbl( dbl, dbl );"
  , "      e = e >> 1;"
  , "    }"
  , "  }"
  , "}"
  ]

scaleWindowed :: CodeGenParams -> Code
scaleWindowed (CodeGenParams{..}) =
  [ "#define TBL(k) (table + (k-1)*3*NLIMBS_P)"
  , ""
  , "// precalculate [ k*g | k <- [1..15] ]"
  , "void " ++ prefix ++ "precalc_expos_window_16( const uint64_t *grp, uint64_t *table ) {"
  , "  " ++ prefix ++ "copy( grp              , TBL( 1) );           //  1*g"
  , "  " ++ prefix ++ "dbl ( TBL(1)           , TBL( 2) );           //  2*g"
  , "  " ++ prefix ++ "dbl ( TBL(2)           , TBL( 4) );           //  4*g"
  , "  " ++ prefix ++ "dbl ( TBL(4)           , TBL( 8) );           //  8*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL( 2) , TBL( 3) );           //  3*g"
  , "  " ++ prefix ++ "dbl ( TBL(3) ,           TBL( 6) );           //  6*g"
  , "  " ++ prefix ++ "dbl ( TBL(6) ,           TBL(12) );           // 12*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL( 4) , TBL( 5) );           //  5*g"
  , "  " ++ prefix ++ "dbl ( TBL(5) ,           TBL(10) );           // 10*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL( 6) , TBL( 7) );           //  7*g"
  , "  " ++ prefix ++ "dbl ( TBL(7) ,           TBL(14) );           // 14*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL( 8) , TBL( 9) );           //  9*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL(10) , TBL(11) );           // 11*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL(12) , TBL(13) );           // 13*g"
  , "  " ++ prefix ++ "add ( TBL(1) , TBL(14) , TBL(15) );           // 15*g"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in " ++ typeName ++ ", and `expo` is a (non-negative) bigint"
  , "// generic windowed algo, 4-bit windows"
  , "void " ++ prefix ++ "scl_windowed(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {"
  , ""
  , "  // precalculate [ k*g | k <- [1..15] ]"
  , "  uint64_t table[15*3*NLIMBS_P];"
  , "  " ++ prefix ++ "precalc_expos_window_16( grp, table );"
  , ""
  , "  " ++ prefix ++ "set_infinity( tgt );           // tgt := infinity"
  , ""
  , "  int s = expo_len - 1;"
  , "  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers"
  , ""
  , "  for(int i=s; i>=0; i--) {"
  , "    uint64_t e = expo[i];"
  , "    for(int j=0; j<16; j++) {"
  , "      // we can skip doubling when infinity"
  , "      if (!" ++ prefix_p ++ "is_zero(tgt+2*NLIMBS_P)) {"
  , "        " ++ prefix ++ "dbl_inplace( tgt );"
  , "        " ++ prefix ++ "dbl_inplace( tgt );"
  , "        " ++ prefix ++ "dbl_inplace( tgt );"
  , "        " ++ prefix ++ "dbl_inplace( tgt );"
  , "      }"
  , "      int k = (e >> 60);"
  , "      if (k) { "
  , "        " ++ prefix ++ "add_inplace( tgt, TBL(k) ); "
  , "      }"
  , "      e = e << 4;"
  , "    }"
  , "  }"
  , "}"
  ]

scaleFpFr :: CodeGenParams -> Code
scaleFpFr (CodeGenParams{..}) =
  [ "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G, and `expo` is in Fr"
  , "void " ++ prefix ++ "scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {"
  , "  " ++ prefix ++ "scl_windowed(expo, grp, tgt, nlimbs);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G, and `expo` is in Fr *in standard repr*"
  , "void " ++ prefix ++ "scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_generic(expo, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G, and `expo` is in Fr *in Montgomery repr*"
  , "void " ++ prefix ++ "scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t expo_std[NLIMBS_R];"
  , "  " ++ prefix_r ++ "to_std(expo, expo_std);"
  , "  " ++ prefix   ++ "scl_generic(expo_std, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G, and `expo` is the same size as Fp"
  , "void " ++ prefix ++ "scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_generic(expo, grp, tgt, NLIMBS_P);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G, and `expo` is a 64 bit (unsigned!) word"
  , "void " ++ prefix ++ "scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t expo_vec[1];"
  , "  expo_vec[0] = expo;"
  , "  " ++ prefix ++ "scl_generic(expo_vec, grp, tgt, 1);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: XCurve -> CodeGenParams -> Code
c_code curve params = concat $ map ("":)
  [ c_begin     curve params
    --
  , scale_by_AB3B curve params
    --
  , normalize            params
  , convertAffine        params
  , isOnCurve      curve params
    --
  , negCurve       params
  , dblCurve curve params
  , if isCurveAZero curve 
      then addCurveA0   params
      else addCurve     params
  , subCurve         params
  , mixedAddCurve    params
    --
  , scaleNaive       params
  , scaleWindowed    params
  , scaleFpFr        params
    --
  , msmCurve          params
  , c_group_fft curve params
  ]

hs_code :: XCurve -> CodeGenParams -> Code
hs_code curve params@(CodeGenParams{..}) = concat $ map ("":)
  [ hsBegin         curve params
  , msm_hs_binding        params
  , fft_hs_binding        params
  , hsSage          curve params
  , hsFFI                 params
  ]

--------------------------------------------------------------------------------

curve_MontProj_c_codegen :: FilePath -> XCurve -> CodeGenParams -> IO ()
curve_MontProj_c_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path_proj)
  let fn_c = tgtdir </> (cFilePath "c" c_path_proj)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code curve params

curve_MontProj_hs_codegen :: FilePath -> XCurve -> CodeGenParams -> IO ()
curve_MontProj_hs_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path_proj)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code curve params

--------------------------------------------------------------------------------

