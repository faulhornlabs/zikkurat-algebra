
-- | Jacobian (or weighted) projective coordinates, Montgomery field representation

{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.MontJac where

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
import Zikkurat.CodeGen.Curve.MSM

--------------------------------------------------------------------------------

c_header :: Curve -> CodeGenParams -> Code
c_header curve@(Curve{..}) cgparams@(CodeGenParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "normalize         ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "normalize_inplace (       uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "copy        ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "from_affine ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "to_affine   ( const uint64_t *src , uint64_t *tgt );"
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
  , "extern void " ++ prefix ++ "madd_jac_aff ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "madd_aff_jac ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "madd_inplace (       uint64_t *tgt , const uint64_t *src2 );"
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
  (msm_c_header curve cgparams)

--------------------------------------------------------------------------------

hsFFI :: Curve -> CodeGenParams -> Code
hsFFI (Curve{..}) (CodeGenParams{..}) = catCode $ 
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
    --
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInProj              , CArgOutProj ] CRetVoid)
  , mkffi "dbl"         $ cfun "dbl"              (CTyp [CArgInProj              , CArgOutProj ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInProj , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInProj , CArgInProj , CArgOutProj ] CRetVoid)
    --
  , mkffi "madd"        $ cfun "madd_jac_aff"     (CTyp [CArgInProj , CArgInAffine , CArgOutProj ] CRetVoid)
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
      , hsTyNameProj   = hsModule hs_path_jac    ++ "." ++         typeName
      , hsTyConProj    = hsModule hs_path_jac    ++ "." ++ "Mk" ++ typeName
      , hsTyNameAffine = hsModule hs_path_affine ++ "." ++         typeName
      , hsTyConAffine  = hsModule hs_path_affine ++ "." ++ "Mk" ++ typeName
      , hsNLimbsP = nlimbs_p
      , hsNLimbsR = nlimbs_r
      }

--------------------------------------------------------------------------------

hsBegin :: Curve -> CodeGenParams -> Code
hsBegin (Curve{..}) (CodeGenParams{..}) =
  [ "-- | " ++ curveName ++ " curve, Jacobian (or weighted) projective coordinates, Montgomery field representation"
  , "--"
  , "-- * NOTE 1: This module is intented to be imported qualified"
  , "--"
  , "-- * NOTE 2: Generated code, do not edit!"
  , "--"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies #-}"
  , "module " ++ hsModule hs_path_jac
  , "  ( " ++ typeName ++ "(..)"
  , "  , primeP , primeR , cofactor , curveA , curveB"
  , "    -- * parameters"
  , "  , genG1 , infinity"
  , "    -- * curve points"
  , "  , coords , mkPoint , mkPointMaybe , unsafeMkPoint"
  , "  , fromAffine , toAffine"
  , "  , normalize"
  , "    -- * predicates"
  , "  , isEqual , isSame"
  , "  , isOnCurve , isInfinity , isInSubgroup"
  , "    -- * addition and doubling"
  , "  , neg , add , madd, dbl , sub"
  , "    -- * scaling"
  , "  , sclFr , sclBig , sclSmall"
  , "    -- * random"
  , "  , rndG1 , rndG1_naive"
  , "    -- * multi-scalar multiplication"
  , "  , msm , msmStd"
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
  , "import " ++ hsModule hs_path_p ++ " ( Fp(..) )"
  , "import " ++ hsModule hs_path_r ++ " ( Fr(..) )"
  , "import qualified " ++ hsModule hs_path_p ++ " as Fp"
  , "import qualified " ++ hsModule hs_path_r ++ " as Fr"
  , "import qualified " ++ hsModule hs_path_r_std 
  , "import qualified " ++ hsModule hs_path_big_p ++ " as BigP"
  , ""
  , "import {-# SOURCE #-} qualified " ++ hsModule hs_path_affine  -- ++ " as Affine" 
  , ""
  , "import           ZK.Algebra.Class.Flat ( FlatArray(..) )"
  , "import qualified ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as F"
  , "import qualified ZK.Algebra.Class.Curve as C"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- | The sizes of the fields Fp, Fr, and the cofactor of the subgroup G1"
  , "primeP, primeR, cofactor :: Integer"
  , "primeP = Fp.prime"
  , "primeR = Fr.prime"
  , "cofactor = " ++ show cofactor
  , ""
  , "-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@"
  , "curveA, curveB :: Integer"
  , "curveA = " ++ show curveA
  , "curveB = " ++ show curveB
  , ""
  , "-- | generator of the r-sized subgroup G1"
  , "genG1 :: " ++ typeName
  , "genG1 = mkPoint (x, y, Fp.one) where"
  , "  x = " ++ show (fst subgroupGen)
  , "  y = " ++ show (snd subgroupGen)
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- | An elliptic curve point, in Jacobian (weighted projective) coordinates"
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "-- | Note: this throws an exception if the point is not on the curve"
  , "mkPoint :: (Fp, Fp, Fp) -> " ++ typeName
  , "mkPoint xyz = case mkPointMaybe xyz of"
  , "  Just pt -> pt"
  , "  Nothing -> error \"mkPoint: point is not on the curve\""
  , ""
  , "mkPointMaybe :: (Fp, Fp, Fp) -> Maybe " ++ typeName
  , "mkPointMaybe xyz = let pt = unsafeMkPoint xyz in"
  , "  case isOnCurve pt of { True -> Just pt ; False -> Nothing }"
  , ""
  , "-- | The point at infinity, @{1 : 1 : 0}@"
  , "infinity :: " ++ typeName
  , "infinity = unsafeMkPoint (Fp.one, Fp.one, Fp.zero)"
  , ""
  , "{-# NOINLINE unsafeMkPoint #-}"
  , "unsafeMkPoint :: (Fp, Fp, Fp) -> " ++ typeName
  , "unsafeMkPoint (MkFp fptr1 , MkFp fptr2 , MkFp fptr3) = unsafePerformIO $ do"
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
  , "coords :: " ++ typeName ++ " -> (Fp, Fp, Fp)"
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
  , "  return (MkFp fptr1, MkFp fptr2, MkFp fptr3)"
  , ""
  , "-- | Returns a uniformly random element /in the subgroup G1/."
  , "-- Note: this is slow, because it uses exponentiation."
  , "rndG1_naive :: IO " ++ typeName
  , "rndG1_naive = do"
  , "  k <- Fr.rnd :: IO Fr"
  , "  return (sclFr k genG1)"
  , "" 
  , "-- | Returns a uniformly random element /in the subgroup G1/"
  , "rndG1 :: IO " ++ typeName
  , "rndG1 = rndG1_naive"
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
  , "     (x,y,z) -> \"{ \" ++ show x ++ \" : \" ++ show y ++ \" : \" ++ show z ++ \" }\""
  , ""
  , "instance L.Flat " ++ typeName ++ " where"
  , "  sizeInBytes  _pxy = " ++ show (8*3*nlimbs_p)
  , "  sizeInQWords _pxy = " ++ show (  3*nlimbs_p)
  , "  withFlat (Mk" ++ typeName ++ " fptr) = withForeignPtr fptr"
  , "  makeFlat = L.makeFlatGeneric Mk" ++ typeName ++ " " ++ show (3*nlimbs_p)
  , ""
  , "instance F.Rnd " ++ typeName ++ " where"
  , "  rndIO = rndG1"
  , ""
  , "instance C.Group " ++ typeName ++ " where"
  , "  grpName _    = \"" ++ curveName ++ " / G1\""
  , "  grpIsUnit    = " ++ hsModule hs_path_jac ++ ".isInfinity"
  , "  grpUnit      = " ++ hsModule hs_path_jac ++ ".infinity"
  , "  grpNormalize = normalize"
  , "  grpNeg       = neg"
  , "  grpDbl       = dbl"
  , "  grpAdd       = add"
  , "  grpSub       = sub"
  , "  grpScale_    = sclSmall"
  , "  grpScale     = sclBig"
  , ""
  , "instance C.Curve " ++ typeName ++ " where"
  , "  curveNamePxy _ = \"" ++ curveName ++ " ( Fp )\""
  , "  type BaseField   " ++ typeName ++ " = Fp"
  , "  type ScalarField " ++ typeName ++ " = Fr"
  , "  isOnCurve   = " ++ hsModule hs_path_jac ++ ".isOnCurve"
  , "  isInifinity = " ++ hsModule hs_path_jac ++ ".isInfinity"
  , "  infinity    = " ++ hsModule hs_path_jac ++ ".infinity"
  , "  subgroupGen = " ++ hsModule hs_path_jac ++ ".genG1"
  , "  scalarMul   = " ++ hsModule hs_path_jac ++ ".sclFr"
  , ""
  , "instance C.ProjCurve " ++ typeName ++ " where"
  , "  type AffinePoint " ++ typeName ++ " = " ++ hsModule hs_path_affine ++ "." ++ typeName
  , "  fromAffine = " ++ hsModule hs_path_jac ++ ".fromAffine"
  , "  toAffine   = " ++ hsModule hs_path_jac ++ ".toAffine"
  , "  coords3    = " ++ hsModule hs_path_jac ++ ".coords"
  , "  mkPoint3   = " ++ hsModule hs_path_jac ++ ".mkPoint"
  , "  mixedAdd   = " ++ hsModule hs_path_jac ++ ".madd"
  , "  "
  , "--------------------------------------------------------------------------------"
  , ""
  , "sclSmall :: Int -> G1 -> G1"
  , "sclSmall k pt"
  , "  | k == 0    = infinity"
  , "  | k < 0     = neg $ sclSmallNonNeg (negate k) pt"
  , "  | otherwise =       sclSmallNonNeg (       k) pt"
  , ""
  , "sclBig :: Integer -> G1 -> G1"
  , "sclBig k pt"
  , "  | k == 0    = infinity"
  , "  | k < 0     = neg $ sclBigNonNeg (fromInteger $ negate k) pt"
  , "  | otherwise =       sclBigNonNeg (fromInteger $        k) pt"
  , ""
  , "--------------------------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------

c_begin :: Curve -> CodeGenParams -> Code
c_begin curve@(Curve{..}) cgparams@(CodeGenParams{..}) =
  [ "// elliptic curve " ++ show curveName ++ " in projective coordinates, Montgomery field representation"
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <string.h>"
  , "#include <stdint.h>"
  , "#include <x86intrin.h>"
  , "#include <assert.h>"
  , "#include <math.h>         // used only for log2()"
  , ""
  , "#include \"" ++ pathBaseName c_path_jac   ++ ".h\""
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
  , "// the generator of the subgroup G1"
  , mkConstArr nlimbs_p (prefix ++ "gen_G1") (map toMontgomery [ fst subgroupGen, snd subgroupGen , 1 ])
  , ""
  , "// the cofactor of the curve subgroup = " ++ show cofactor
  , mkConst nlimbs_p (prefix ++ "cofactor") cofactor
  , ""
  , "//------------------------------------------------------------------------------"
  ] 
  where
    toMontgomery x = mod ( 2^(64*nlimbs_p) * x ) curveFp

--------------------------------------------------------------------------------

scale_by_A ::  Curve -> CodeGenParams -> Code
scale_by_A (Curve{..}) (CodeGenParams{..}) = case curveA of

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
       , "  " ++ prefix_p ++ "scl( " ++ show curveA ++ ", src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "scl_inplace( tgt, " ++ show curveA ++ " );"
       , "}"
       ]

----------------------------------------

scale_by_B ::  Curve -> CodeGenParams -> Code
scale_by_B (Curve{..}) (CodeGenParams{..}) = case curveB of

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
       , "  " ++ prefix_p ++ "scl( " ++ show curveB ++ ", src, tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "scl_inplace( tgt, " ++ show curveB ++ " );"
       , "}"
       ]

--------------------------------------------------------------------------------

normalize :: Curve -> CodeGenParams -> Code
normalize (Curve{..}) (CodeGenParams{..}) =
  [ "void " ++ prefix ++ "normalize( const uint64_t *src1, uint64_t *tgt ) {"
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
  , "      uint64_t zinv [" ++ show nlimbs_p ++ "];"
  , "      uint64_t zinv2[" ++ show nlimbs_p ++ "];"
  , "      uint64_t zinv3[" ++ show nlimbs_p ++ "];"
  , "      " ++ prefix_p ++ "inv( Z1, zinv );"
  , "      " ++ prefix_p ++ "sqr( zinv, zinv2 );"
  , "      " ++ prefix_p ++ "mul( zinv, zinv2, zinv3 );"
  , "      " ++ prefix_p ++ "mul( X1, zinv2, X3 );"
  , "      " ++ prefix_p ++ "mul( Y1, zinv3, Y3 );"
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

convertAffine :: Curve -> CodeGenParams -> Code
convertAffine (Curve{..}) (CodeGenParams{..}) = 
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
  , "    uint64_t zinv [" ++ show nlimbs_p ++ "];"
  , "    uint64_t zinv2[" ++ show nlimbs_p ++ "];"
  , "    uint64_t zinv3[" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "inv( Z1, zinv );"
  , "    " ++ prefix_p ++ "mul( zinv, zinv , zinv2 );"
  , "    " ++ prefix_p ++ "mul( zinv, zinv2, zinv3 );"
  , "    " ++ prefix_p ++ "mul( X1, zinv2, X3 );"
  , "    " ++ prefix_p ++ "mul( Y1, zinv3, Y3 );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  if (tgt != src1) { memcpy( tgt, src1, " ++ show (8*3*nlimbs_p) ++ " ); }"
  , "}"  
  ]

isOnCurve :: Curve -> CodeGenParams -> Code
isOnCurve (Curve{..}) (CodeGenParams{..}) = 
  [ "uint8_t " ++ prefix ++ "is_infinity ( const uint64_t *src1 ) {"
  , "  if ( ( " ++ prefix_p ++ "is_zero( Z1 )) &&"
  , "       (!" ++ prefix_p ++ "is_zero( X1 )) &&"
  , "       (!" ++ prefix_p ++ "is_zero( Y1 )) ) {"
  , "    // for Z=0 we have the equation Y^2 = X^3"
  , "    uint64_t XX [" ++ show nlimbs_p ++ "];"
  , "    uint64_t XXX[" ++ show nlimbs_p ++ "];"
  , "    uint64_t YY [" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "sqr(X1, XX);"
  , "    " ++ prefix_p ++ "mul(X1, XX, XXX);"
  , "    " ++ prefix_p ++ "sqr(Y1, YY);"
  , "    return " ++ prefix_p ++ "is_equal( YY, XXX );"
  , "  }"
  , "  else {"
  , "    return 0;"
  , "  }"
  , "}"
  , ""
  , "// note: In Jacobian coordinates, the point at infinity is [1:1:0]"
  , "void " ++ prefix ++ "set_infinity ( uint64_t *tgt ) {"
  , "  " ++ prefix_p ++ "set_one ( X3 );"
  , "  " ++ prefix_p ++ "set_one ( Y3 );"
  , "  " ++ prefix_p ++ "set_zero( Z3 );"
  , "}"
  , ""
  , "// checks the curve equation"
  , "//   y^2 == x^3 + A*x*z^4 + B*z^6"
  , "uint8_t " ++ prefix ++ "is_on_curve ( const uint64_t *src1 ) {"
  , "  uint64_t ZZ2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t ZZ4[" ++ show nlimbs_p ++ "];"
  , "  uint64_t acc[" ++ show nlimbs_p ++ "];"
  , "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
  , "  " ++ prefix_p ++ "sqr( Y1, acc );             // Y^2"
  , "  " ++ prefix_p ++ "neg_inplace( acc );         // -Y^2"
  , "  " ++ prefix_p ++ "sqr( X1, tmp );             // X^2"
  , "  " ++ prefix_p ++ "mul_inplace( tmp, X1 );     // X^3"
  , "  " ++ prefix_p ++ "add_inplace( acc, tmp );    // - Y^2 + X^3"
  , "  " ++ prefix_p ++ "sqr( Z1 , ZZ2 );            // Z^2"
  , "  " ++ prefix_p ++ "sqr( ZZ2, ZZ4 );            // Z^4"
  ] ++ 
  (if curveA == 0 then [] else 
    [ "  " ++ prefix_p ++ "mul( X1, ZZ4, tmp );          // X*Z^4"
    , "  " ++ prefix   ++ "scale_by_A_inplace( tmp );   // A*X*Z^4"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2 + X^3 + A*X*Z^4"
    ]
  ) ++ 
  (if curveB == 0 then [] else 
    [ "  " ++ prefix_p ++ "mul( ZZ2, ZZ4, tmp );        // Z^6"
    , "  " ++ prefix   ++ "scale_by_B_inplace( tmp );   // B*Z^6"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2 + X^3 + A*X*Z^4 + B*Z^6"
    ]
  ) ++
  [ "  return (" ++ prefix_p ++ "is_zero( acc ) &&"
  , "           ( (!" ++ prefix_p ++ "is_zero( Z1 )) || "
  , "             (!" ++ prefix_p ++ "is_zero( Y1 )) ) );"
  , "}"
  , ""
  , "// checks whether the given point is in the subgroup G1"
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

negCurve :: Curve -> CodeGenParams -> Code
negCurve (Curve{..}) (CodeGenParams{..}) =
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
 -- actuall this is the same as below, we don't need to reimplement it
 formula from https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#doubling-dbl-2009-l
 assumptions: a = 0
      A = X1^2
      B = Y1^2
      C = B^2
      D = 2*((X1+B)^2-A-C)
      E = 3*A
      F = E^2
      X3 = F-2*D
      Y3 = E*(D-X3)-8*C
      Z3 = 2*Y1*Z1

dblCurveA0 :: Curve -> CodeGenParams -> Code
dblCurveA0  = dblCurve
-}

----------------------------------------

{-
  formula from <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-2007-bl>
      XX = X1^2
      YY = Y1^2
      YYYY = YY^2
      ZZ = Z1^2
      S = 2*((X1+YY)^2-XX-YYYY)
      M = 3*XX+a*ZZ^2
      T = M^2-2*S
      X3 = T
      Y3 = M*(S-T)-8*YYYY
      Z3 = (Y1+Z1)^2-YY-ZZ
-}
dblCurve :: Curve -> CodeGenParams -> Code
dblCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// doubles an elliptic curve point" 
  , "// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-2007-bl>"
  , "void " ++ prefix ++ "dbl( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t   XX[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   YY[" ++ show nlimbs_p ++ "];"
  , "  uint64_t YYYY[" ++ show nlimbs_p ++ "];"
  , "  uint64_t   ZZ[" ++ show nlimbs_p ++ "];"
  , "  uint64_t    S[" ++ show nlimbs_p ++ "];"
  , "  uint64_t    M[" ++ show nlimbs_p ++ "];"
  , "  uint64_t    T[" ++ show nlimbs_p ++ "];"
  , "  " ++ prefix_p ++ "sqr( X1, XX );           // XX = X1^2"
  , "  " ++ prefix_p ++ "sqr( Y1, YY );           // YY = Y1^2"
  , "  " ++ prefix_p ++ "sqr( YY, YYYY );         // YYYY = Y1^4"
  , "  " ++ prefix_p ++ "sqr( Z1, ZZ );           // ZZ = Z1^2"
  , "  " ++ prefix_p ++ "add( X1, YY , S );       // = (X1+YY)"
  , "  " ++ prefix_p ++ "sqr_inplace( S );        // = (X1+YY)^2"
  , "  " ++ prefix_p ++ "sub_inplace( S, XX );    // = (X1+YY)^2 - XX"
  , "  " ++ prefix_p ++ "sub_inplace( S, YYYY );  // = (X1+YY)^2 - XX -YYYY"
  , "  " ++ prefix_p ++ "add_inplace( S, S );     // = S = 2*((X1+YY)^2-XX-YYYY)"
  , "  " ++ prefix_p ++ "add( XX, XX, M );        // = 2*XX"
  , "  " ++ prefix_p ++ "add_inplace( M, XX );    // M = 3*XX"   
  ] ++ (if curveA == 0 then [] else 
    [ "  " ++ prefix_p ++ "sqr( ZZ , T );              // = ZZ^2"
    , "  " ++ prefix   ++ "scale_by_A_inplace( T );    // = a*ZZ^2"
    , "  " ++ prefix_p ++ "add_inplace ( M , T );      // M = 3*XX + a*ZZ^2"
    ]) ++
  [ "  " ++ prefix_p ++ "sqr( M, T );                // T = M^2"
  , "  " ++ prefix_p ++ "sub_inplace( T, S );        // T = M^2 - S"
  , "  " ++ prefix_p ++ "sub_inplace( T, S );        // T = M^2 - 2*S"
  , "  " ++ prefix_p ++ "copy( T, X3 );              // X3  = T"
  , "  " ++ prefix_p ++ "add( Z1, Y1, Z3 );          // Z3  = Y1+Z1"
  , "  " ++ prefix_p ++ "sqr_inplace( Z3 );          // Z3  = (Y1+Z1)^2"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, YY );      // Z3  = (Y1+Z1)^2 - YY"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, ZZ );      // Z3  = (Y1+Z1)^2 - YY - ZZ"
  , "  " ++ prefix_p ++ "sub( S, T, Y3 );            // Y3  = S - T"
  , "  " ++ prefix_p ++ "mul_inplace( Y3, M );       // Y3  = M*(S - T)"
  , "  " ++ prefix_p ++ "add_inplace( YYYY , YYYY ); // 2 * YYYY"
  , "  " ++ prefix_p ++ "add_inplace( YYYY , YYYY ); // 4 * YYYY"
  , "  " ++ prefix_p ++ "add_inplace( YYYY , YYYY ); // 8 * YYYY"
  , "  " ++ prefix_p ++ "sub_inplace( Y3 , YYYY);    // Y3  = M*(S - T) - 8*YYYY"
  , "}"
  , ""
  , "// doubles an elliptic curve point" 
  , "void " ++ prefix ++ "dbl_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "dbl( tgt , tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

{-
formula from <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl>

note: this does not work for adding the point at infinity!
neither for doubling, it seems

      Z1Z1 = Z1^2
      Z2Z2 = Z2^2
      U1 = X1*Z2Z2
      U2 = X2*Z1Z1
      S1 = Y1*Z2*Z2Z2
      S2 = Y2*Z1*Z1Z1
      H = U2-U1
      I = (2*H)^2
      J = H*I
      r = 2*(S2-S1)
      V = U1*I
      X3 = r^2-J-2*V
      Y3 = r*(V-X3)-2*S1*J
      Z3 = ((Z1+Z2)^2-Z1Z1-Z2Z2)*H
-}

-- TODO: implement A=0 special case
addCurveA0 = addCurve 

addCurve :: Curve -> CodeGenParams -> Code
addCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// adds two elliptic curve points" 
  , "// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl>"
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity(src1)) {"
  , "    if (tgt != src2) { memcpy( tgt, src2, " ++ show (8*3*nlimbs_p) ++ "); }"
  , "    return;"
  , "  }"
  , "  if (" ++ prefix ++ "is_infinity(src2)) {"
  , "    if (tgt != src1) { memcpy( tgt, src1, " ++ show (8*3*nlimbs_p) ++ "); }"
  , "    return;"
  , "  }"
  , "  uint64_t Z1Z1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t Z2Z2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t U1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t U2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t S1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t S2[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  H[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  I[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  J[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  r[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  V[" ++ show nlimbs_p ++ "];"  
  , "  " ++ prefix_p ++ "sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2"
  , "  " ++ prefix_p ++ "sqr( Z2, Z2Z2 );           // Z2Z2 = Z2^2"
  , "  " ++ prefix_p ++ "mul( X1, Z2Z2 , U1 );      // U1 = X1*Z2Z2"
  , "  " ++ prefix_p ++ "mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1"
  , "  " ++ prefix_p ++ "mul( Y1, Z2 , S1 );        //    = Y1 * Z2"
  , "  " ++ prefix_p ++ "mul_inplace(  S1, Z2Z2 );  // S1 = Y1 * Z2 * Z2Z2"
  , "  " ++ prefix_p ++ "mul( Y2, Z1 , S2 );        //    = Y2 * Z1"
  , "  " ++ prefix_p ++ "mul_inplace(  S2, Z1Z1 );  // S2 = Y2 * Z1 * Z1Z1"
  , "  " ++ prefix_p ++ "sub( U2, U1, H );          // H  = U2-U1"
  , "  if (" ++ prefix_p ++ "is_zero( H )) {"
  , "    // X1/Z1^2 == X2/Z2^2"
  , "    // so either Y1/Z1^3 == Y2/Z2^3, in which case it's a doubling"
  , "    // or not, in which case Y1/Z1^3 == - Y2/Z2^3 and the result is infinity"
  , "    if (" ++ prefix_p ++ "is_equal( S1, S2)) {"
  , "      // Y1/Z1^3 == Y2/Z2^3"
  , "      " ++ prefix ++ "dbl( src1, tgt );"
  , "      return;"
  , "    }"
  , "    else {"
  , "      // Y1/Z1^3 != Y2/Z2^3"
  , "      " ++ prefix ++ "set_infinity( tgt );"
  , "      return;"
  , "    }"
  , "  }"
  , "  " ++ prefix_p ++ "add( H, H, I );            //    = 2*H"
  , "  " ++ prefix_p ++ "sqr_inplace( I );          // I  = (2*H)^2"
  , "  " ++ prefix_p ++ "mul( H, I, J );            // J  = H*I"
  , "  " ++ prefix_p ++ "sub( S2, S1, r );          //    = S2-S1"
  , "  " ++ prefix_p ++ "add_inplace( r, r );       // r  = 2*(S2-S1)"
  , "  " ++ prefix_p ++ "mul( U1, I, V );           // V  = U1*I"
  , "  " ++ prefix_p ++ "sqr( r, X3 );              //    = r^2"
  , "  " ++ prefix_p ++ "sub_inplace( X3, J );      //    = r^2 - J"
  , "  " ++ prefix_p ++ "sub_inplace( X3, V );      //    = r^2 - J - V"
  , "  " ++ prefix_p ++ "sub_inplace( X3, V );      // X3 = r^2 - J - 2*V"
  , "  " ++ prefix_p ++ "sub( V, X3, Y3 );          //    = V-X3"
  , "  " ++ prefix_p ++ "mul_inplace( Y3, r );      //    = r*(V-X3)"
  , "  " ++ prefix_p ++ "mul_inplace( J, S1 );      // J := S1*J"
  , "  " ++ prefix_p ++ "sub_inplace( Y3, J );      //    = r*(V-X3) - S1*J"
  , "  " ++ prefix_p ++ "sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*S1*J"
  , "  " ++ prefix_p ++ "add( Z1, Z2, Z3 );         //    = Z1+Z2"
  , "  " ++ prefix_p ++ "sqr_inplace( Z3 );         //    = (Z1+Z2)^2"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, Z1Z1 );   //    = (Z1+Z2)^2-Z1Z1"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, Z2Z2 );   //    = (Z1+Z2)^2-Z1Z1-Z2Z2"
  , "  " ++ prefix_p ++ "mul_inplace( Z3, H );      // Z3 = ((Z1+Z2)^2-Z1Z1-Z2Z2)*H"
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "add( tgt, src2, tgt);"
  , "}"
  ]

subCurve :: Curve -> CodeGenParams -> Code
subCurve (Curve{..}) (CodeGenParams{..}) =
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

https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-madd-2007-bl

note: this formula does not seem to work for any of the corner cases...

assumption: Z2 == 1

      Z1Z1 = Z1^2
      U2 = X2*Z1Z1
      S2 = Y2*Z1*Z1Z1
      H = U2-X1
      HH = H^2
      I = 4*HH
      J = H*I
      r = 2*(S2-Y1)
      V = X1*I
      X3 = r^2-J-2*V
      Y3 = r*(V-X3)-2*Y1*J
      Z3 = (Z1+H)^2-Z1Z1-HH

-}

mixedAddCurve :: Curve -> CodeGenParams -> Code
mixedAddCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// adds a Jacobian projective point (src1) to an affine point (src2)"
  , "// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-madd-2007-bl>"
  , "void " ++ prefix ++ "madd_jac_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity( src1 )) {"
  , "    // the formula is not valid for this case"
  , "    " ++ prefix ++ "from_affine( src2 , tgt );"
  , "    return;"
  , "  }"
  , "  if (" ++ prefix_affine ++ "is_infinity( src2 )) {"
  , "    " ++ prefix ++ "copy( src1 , tgt );"
  , "    return;"
  , "  }"
  , "  uint64_t Z1Z1[" ++ show nlimbs_p ++ "];"
  , "  uint64_t U2[" ++ show nlimbs_p ++ "];"
  , "  uint64_t S2[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  H[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t HH[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  I[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  J[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  r[" ++ show nlimbs_p ++ "];"  
  , "  uint64_t  V[" ++ show nlimbs_p ++ "];"  
  , "  " ++ prefix_p ++ "sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2"
  , "  " ++ prefix_p ++ "mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1"
  , "  " ++ prefix_p ++ "mul( Y2, Z1 , S2 );        //    = Y2 * Z1"
  , "  " ++ prefix_p ++ "mul_inplace( S2, Z1Z1 );   // S2 = Y2 * Z1 * Z1Z1"
  , "  " ++ prefix_p ++ "sub( U2, X1, H );          // H  = U2-X1"
  , "  " ++ prefix_p ++ "sqr( H, HH );              // HH = H^2"
  , "  " ++ prefix_p ++ "add( HH, HH, I );          //    = 2*HH"
  , "  " ++ prefix_p ++ "add_inplace( I, I );       // I  = 4*HH"
  , "  " ++ prefix_p ++ "mul( H, I, J );            // J  = H*I"
  , "  " ++ prefix_p ++ "sub( S2, Y1, r );          //    = S2-Y1"
  , "  if (" ++ prefix_p ++ "is_zero(H)) {"
  , "    // H=0  <==>  X1/Z1^2 = X2"
  , "    // either a doubling or the result is infinity"
  , "    if (" ++ prefix_p ++ "is_zero(r)) {"
  , "      // r=0  <==>  Y1/Z1^2 = Y2"
  , "      // it's a doubling"
  , "      " ++ prefix ++ "dbl( src1, tgt );"
  , "      return;"
  , "    }"
  , "    else {"
  , "      // X1/Z1^2 = X2 but Y1/Z1^2 /= Y2"
  , "      // so the result must be infinity"
  , "      " ++ prefix ++ "set_infinity( tgt );"
  , "      return;"
  , "    }"
  , "  }"
  , "  " ++ prefix_p ++ "add_inplace( r, r );       // r  = 2*(S2-Y1)"
  , "  " ++ prefix_p ++ "mul( X1, I, V );           // V  = X1*I"
  , "  " ++ prefix_p ++ "sqr( r, X3 );              //    = r^2"
  , "  " ++ prefix_p ++ "sub_inplace( X3, J );      //    = r^2 - J"
  , "  " ++ prefix_p ++ "sub_inplace( X3, V );      //    = r^2 - J - V"
  , "  " ++ prefix_p ++ "sub_inplace( X3, V );      // X3 = r^2 - J - 2*V"
  , "  " ++ prefix_p ++ "mul_inplace( J, Y1 );      // J := Y1*J - careful, in the next row we possibly overwrite Y1!"
  , "  " ++ prefix_p ++ "sub( V, X3, Y3 );          //    = V-X3"
  , "  " ++ prefix_p ++ "mul_inplace( Y3, r );      // Y3 = r*(V-X3)"
  , "  " ++ prefix_p ++ "sub_inplace( Y3, J );      //    = r*(V-X3) - Y1*J"
  , "  " ++ prefix_p ++ "sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*Y1*J"
  , "  " ++ prefix_p ++ "add( Z1, H , Z3 );         //    = Z1+H"
  , "  " ++ prefix_p ++ "sqr_inplace( Z3 );         //    = (Z1+H)^2"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, Z1Z1 );   //    = (Z1+H)^2-Z1Z1"
  , "  " ++ prefix_p ++ "sub_inplace( Z3, HH );     // Z3 = (Z1+H)^2-Z1Z1-HH"
  , "}"
  , ""
  , "// adds an affine point (src1) to a projective one (src2)"
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb"
  , "void " ++ prefix ++ "madd_aff_jac( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  " ++ prefix ++ "madd_jac_aff( src2, src1, tgt );"
  , "}"
  , ""
  , "// adds to a projective point (tgt) an affine point (src2), in place"
  , "void " ++ prefix ++ "madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "madd_jac_aff( tgt, src2, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

scaleNaive :: Curve -> CodeGenParams -> Code
scaleNaive (Curve{..}) (CodeGenParams{..}) =
  [ "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint"
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

scaleWindowed :: Curve -> CodeGenParams -> Code
scaleWindowed (Curve{..}) (CodeGenParams{..}) =
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
  , "// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint"
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

scaleFpFr :: Curve -> CodeGenParams -> Code
scaleFpFr (Curve{..}) (CodeGenParams{..}) =
  [ "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr"
  , "void " ++ prefix ++ "scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {"
  , "  " ++ prefix ++ "scl_windowed(expo, grp, tgt, nlimbs);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr *in standard repr*"
  , "void " ++ prefix ++ "scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_generic(expo, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr *in Montgomery repr*"
  , "void " ++ prefix ++ "scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t expo_std[NLIMBS_R];"
  , "  " ++ prefix_r ++ "to_std(expo, expo_std);"
  , "  " ++ prefix   ++ "scl_generic(expo_std, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is the same size as Fp"
  , "void " ++ prefix ++ "scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_generic(expo, grp, tgt, NLIMBS_P);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word"
  , "void " ++ prefix ++ "scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t expo_vec[1];"
  , "  expo_vec[0] = expo;"
  , "  " ++ prefix ++ "scl_generic(expo_vec, grp, tgt, 1);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: Curve -> CodeGenParams -> Code
c_code curve params = concat $ map ("":)
  [ c_begin     curve params
    --
  , scale_by_A  curve params
  , scale_by_B  curve params
    --
  , normalize     curve params
  , convertAffine curve params
  , isOnCurve     curve params
    --
  , negCurve curve params
  , dblCurve  curve params
  , if curveA curve == 0
      then addCurveA0 curve params
      else addCurve   curve params
  , subCurve curve params
  , mixedAddCurve curve params
    --
  , scaleNaive    curve params
  , scaleWindowed curve params
  , scaleFpFr     curve params
    --
  , msmCurve      curve params
  ]

hs_code :: Curve -> CodeGenParams -> Code
hs_code curve params@(CodeGenParams{..}) = concat $ map ("":)
  [ hsBegin        curve params
 -- , hsMiscTmp
 -- , hsConvert      curve params
  , msm_hs_binding curve params
  , hsFFI          curve params
  ]

--------------------------------------------------------------------------------

curve_MontJac_c_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontJac_c_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path_jac)
  let fn_c = tgtdir </> (cFilePath "c" c_path_jac)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header curve params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code curve params

curve_MontJac_hs_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontJac_hs_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path_jac)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code curve params

--------------------------------------------------------------------------------

{-
testCurve :: Curve
testCurve = bls12_381_curve

testCodeGenParams :: CodeGenParams
testCodeGenParams = CodeGenParams
  { prefix        = "bls12_381_G1_"                                             -- prefix for C names
  , prefix_p      = "bls12_381_p_mont_"                                         -- prefix for C names for Fp
  , prefix_r      = "bls12_381_q_mont_"                                         -- prefix for C names for Fq
  , nlimbs_p      = 6                                                           -- number of 64-bit limbs in p
  , nlimbs_r      = 4                                                           -- number of 64-bit limbs in r
  , c_path        = Path ["curves","g1","proj","bls12_381_G1_proj"]             -- path of the C file
  , hs_path       = Path ["ZK","Algebra","Curves","BLS12_381","G1","Proj"]      -- path of the Haskell module
  , hs_path_p     = Path ["ZK","Algebra","Curves","BLS12_381","Mont","Fp"]      -- path of the Haskell module for Fp
  , hs_path_r     = Path ["ZK","Algebra","Curves","BLS12_381","Mont","Fr"]      -- path of the Haskell module for Fr
  , c_basename_p  = "bls12_381_p_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r  = "bls12_381_r_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName      = "G1"                                                        -- the name of the haskell type for curve points
  }
-}

--------------------------------------------------------------------------------

