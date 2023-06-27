
-- | Affine coordinates, Montgomery field representation

{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.MontAffine where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc

import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Curve.CurveFFI

--------------------------------------------------------------------------------

c_header :: Curve -> CodeGenParams -> Code
c_header (Curve{..}) (CodeGenParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "copy( const uint64_t *src , uint64_t *tgt );"
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
  , "extern void " ++ prefix ++ "scl_generic( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  , "extern void " ++ prefix ++ "scl_Fr     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_big    ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_small  (       uint64_t  kst , const uint64_t *src , uint64_t *tgt );"
  ]

--------------------------------------------------------------------------------

hsFFI :: Curve -> CodeGenParams -> Code
hsFFI (Curve{..}) (CodeGenParams{..}) = catCode $ 
  [ mkffi "isOnCurve"    $ cfun "is_on_curve"     (CTyp [CArgInAffine] CRetBool)
  , mkffi "isInfinity"   $ cfun "is_infinity"     (CTyp [CArgInAffine] CRetBool)
  , mkffi "isInSubgroup" $ cfun "is_in_subgroup"  (CTyp [CArgInAffine] CRetBool)
    --
  , mkffi "isEqual"      $ cfun "is_equal"        (CTyp [CArgInAffine, CArgInAffine ] CRetBool)
  , mkffi "isSame"       $ cfun "is_same"         (CTyp [CArgInAffine, CArgInAffine ] CRetBool)
    --
  , mkffi "neg"          $ cfun "neg"              (CTyp [CArgInAffine                , CArgOutAffine ] CRetVoid)
  , mkffi "dbl"          $ cfun "dbl"              (CTyp [CArgInAffine                , CArgOutAffine ] CRetVoid)
  , mkffi "add"          $ cfun "add"              (CTyp [CArgInAffine , CArgInAffine , CArgOutAffine ] CRetVoid)
  , mkffi "sub"          $ cfun "sub"              (CTyp [CArgInAffine , CArgInAffine , CArgOutAffine ] CRetVoid)
    --
  , mkffi "sclFr"          $ cfun "scl_Fr"           (CTyp [CArgInScalarR , CArgInAffine , CArgOutAffine ] CRetVoid)
  , mkffi "sclBigNonNeg"   $ cfun "scl_big"          (CTyp [CArgInBigIntP , CArgInAffine , CArgOutAffine ] CRetVoid)
  , mkffi "sclSmallNonNeg" $ cfun "scl_small"        (CTyp [CArgInt       , CArgInAffine , CArgOutAffine ] CRetVoid)
  ]
  where
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

-- | @.hs-boot@ file
hsBoot  :: Curve -> CodeGenParams -> Code
hsBoot (Curve{..}) (CodeGenParams{..}) =
  [ "module " ++ hsModule hs_path ++ " where"
  , ""
  , "import Data.Word"
  , "import Foreign.ForeignPtr"
  , ""
  , "-- | An elliptic curve point, in affine coordinates"
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  ]

hsBegin :: Curve -> CodeGenParams -> Code
hsBegin (Curve{..}) (CodeGenParams{..}) =
  [ "-- NOTE 1: This module is intented to be imported qualified"
  , "-- NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeFamilies #-}"
  , "module " ++ hsModule hs_path
  , "  ( " ++ typeName ++ "(..)"
  , "  , primeP , primeR , cofactor , curveA , curveB"
  , "  , genG1 , infinity"
  , "  , coords , mkPoint , mkPointMaybe , unsafeMkPoint"
  , "  , isEqual , isSame"
  , "  , isInfinity , isOnCurve , isInSubgroup"
  , "  , neg , add , dbl , sub"
  , "  , sclFr , sclBig , sclSmall"
  , "  , rndG1"
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
  , "import qualified " ++ hsModule hs_path_big_p ++ " as BigP"
  , "import qualified " ++ hsModule hs_path_proj  ++ " as Proj    -- note: be careful with cyclic imports!"
  , ""
  , "import qualified ZK.Algebra.Class.Field as F"
  , "import qualified ZK.Algebra.Class.Curve as C"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
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
  , "genG1 = mkPoint (x, y) where"
  , "  x = " ++ show (fst subgroupGen)
  , "  y = " ++ show (snd subgroupGen)
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- | An elliptic curve point, in affine coordinates"
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "-- | Note: this throws an exception if the point is not on the curve"
  , "mkPoint :: (Fp, Fp) -> " ++ typeName
  , "mkPoint xyz = case mkPointMaybe xyz of"
  , "  Just pt -> pt"
  , "  Nothing -> error \"mkPoint: point is not on the curve\""
  , ""
  , "mkPointMaybe :: (Fp, Fp) -> Maybe " ++ typeName
  , "mkPointMaybe xyz = let pt = unsafeMkPoint xyz in"
  , "  case isOnCurve pt of { True -> Just pt ; False -> Nothing }"
  , ""
  , "{-# NOINLINE unsafeMkPoint #-}"
  , "unsafeMkPoint :: (Fp, Fp) -> " ++ typeName
  , "unsafeMkPoint (MkFp fptr1 , MkFp fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (2*nlimbs_p)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        copyBytes (        ptr3 " ++                      "  ) ptr1 " ++ show (8*nlimbs_p)
  , "        copyBytes (plusPtr ptr3 " ++ show (  8*nlimbs_p) ++ ") ptr2 " ++ show (8*nlimbs_p)
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "set_infinity\" c_" ++ prefix ++ "set_infinity :: Ptr Word64 -> IO ()"
  , ""
  , "-- | The point at infinity (represented as the special string @0xffff...ffff@)"
  , "{-# NOINLINE infinity #-}"
  , "infinity :: " ++ typeName
  , "infinity = unsafePerformIO $ do"
  , "  fptr <- mallocForeignPtrArray " ++ show (2*nlimbs_p)
  , "  withForeignPtr fptr $ \\ptr -> do"
  , "    c_" ++ prefix ++ "set_infinity ptr"
  , "  return (Mk" ++ typeName ++ " fptr)"
  , ""
  , "{-# NOINLINE coords #-}"
  , "-- | Affine coordinates (TODO: handle the point at infinity)"
  , "coords :: " ++ typeName ++ " -> (Fp, Fp)"
  , "coords (Mk" ++ typeName ++ " fptr3) = unsafePerformIO $ do"
  , "  fptr1 <- mallocForeignPtrArray " ++ show (nlimbs_p)
  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs_p)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        copyBytes ptr1 (        ptr3 " ++                      "  ) " ++ show (8*nlimbs_p)
  , "        copyBytes ptr2 (plusPtr ptr3 " ++ show (  8*nlimbs_p) ++ ") " ++ show (8*nlimbs_p)
  , "  return (MkFp fptr1, MkFp fptr2)"
  , ""
  , "-- | Returns a uniformly random element /in the subgroup G1/"
  , "rndG1 :: IO " ++ typeName
  , "rndG1 = Proj.toAffine <$> Proj.rndG1"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "instance C.StrictEq " ++ typeName ++ " where"
  , "  (===) = isSame"
  , ""
  , "instance Eq " ++ typeName ++ " where"
  , "  (==) = isEqual"
  , ""
  , "instance Show " ++ typeName ++ " where"
  , "  show pt" 
  , "    | isInfinity pt = \"<point-at-infinity>\""
  , "    | otherwise = case coords pt of"
  , "        (x,y) -> \"( \" ++ show x ++ \" , \" ++ show y ++ \" )\""
  , ""
  , "instance F.Rnd " ++ typeName ++ " where"
  , "  rndIO = rndG1"
  , ""
  , "instance C.Group " ++ typeName ++ " where"
  , "  grpName _    = \"" ++ curveName ++ " / G1\""
  , "  grpIsUnit    = isInfinity"
  , "  grpUnit      = infinity"
  , "  grpNormalize = id"
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
  , "  isOnCurve   = " ++ hsModule hs_path_affine ++ ".isOnCurve"
  , "  isInifinity = " ++ hsModule hs_path_affine ++ ".isInfinity"
  , "  infinity    = " ++ hsModule hs_path_affine ++ ".infinity"
  , "  subgroupGen = " ++ hsModule hs_path_affine ++ ".genG1"
  , "  scalarMul   = " ++ hsModule hs_path_affine ++ ".sclFr"
  , ""
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
  where
    hs_path_proj = pathReplaceBaseName "Proj" hs_path

--------------------------------------------------------------------------------

c_begin :: Curve -> CodeGenParams -> Code
c_begin curve@(Curve{..}) cgparams@(CodeGenParams{..}) =
  [ "// elliptic curve " ++ show curveName ++ " in affine coordinates, Montgomery field representation"
  , "//"
  , "// NOTES:"
  , "//  - generated code, do not edit!"
  , "//  - the point at infinity is represented by the special string 0xffff ..fffff"
  , "//    this is not a valid value for prime fields, so it's OK as long as we always check for it"
  , ""
  , "#include <string.h>"
  , "#include <stdint.h>"
  , "#include <x86intrin.h>"
  , ""
  , "#include \"" ++ pathBaseName c_path_affine ++ ".h\""
  , "#include \"" ++ pathBaseName c_path_proj   ++ ".h\""
  , "#include \"" ++ c_basename_p  ++ ".h\""
  , "#include \"" ++ c_basename_r  ++ ".h\""
  , ""
  , "#define NLIMBS_P " ++ show nlimbs_p
  , "#define NLIMBS_R " ++ show nlimbs_r
  , ""
  , "#define X1 (src1)"
  , "#define Y1 (src1 + " ++ show (  nlimbs_p) ++ ")"
  , ""
  , "#define X2 (src2)"
  , "#define Y2 (src2 + " ++ show (  nlimbs_p) ++ ")"
  , ""
  , "#define X3 (tgt)"
  , "#define Y3 (tgt + " ++ show (  nlimbs_p) ++ ")"
  , ""
  , "// the generator of the subgroup G1"
  , mkConstArr nlimbs_p (prefix ++ "gen_G1") (map toMontgomery [ fst subgroupGen, snd subgroupGen ])
  , ""
  , "// the cofactor of the curve subgroup = " ++ show cofactor
  , mkConst nlimbs_p (prefix ++ "cofactor") cofactor
  , ""
  , "// the constants A and B of the equation"
  , mkConst nlimbs_p (prefix ++ "const_A") (toMontgomery curveA)
  , mkConst nlimbs_p (prefix ++ "const_B") (toMontgomery curveB)
  , ""
  , "//------------------------------------------------------------------------------"
  , ""
  , "void " ++ prefix ++ "set_ffff( uint64_t *tgt ) {"
  , "  memset( tgt, 0xff, " ++ show (8*nlimbs_p) ++ " );"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_ffff( const uint64_t *src ) {"
  , "  return ( " ++ intercalate " && " 
         [ "(src[" ++ show i ++ "] + 1 == 0)" | i <-[0..nlimbs_p-1] ] ++ " );"
  , "}"
  ] 
  where
    toMontgomery x = mod ( 2^(64*nlimbs_p) * x ) curveFp

--------------------------------------------------------------------------------

isOnCurve :: Curve -> CodeGenParams -> Code
isOnCurve (Curve{..}) (CodeGenParams{..}) = 
  [ "// checks whether two curve points are equal"
  , "uint8_t " ++ prefix ++ "is_equal( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  return ( " ++ prefix_p ++ "is_equal( X1, X2 ) &&"
  , "           " ++ prefix_p ++ "is_equal( Y1, Y2 ) );"
  , "}"
  , ""
  , "// checks whether the underlying representation is the same"
  , "uint8_t " ++ prefix ++ "is_same( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  return " ++ prefix ++ "is_equal( src1, src2 );"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_infinity ( const uint64_t *src1 ) {"
  , "  return ( " ++ prefix ++ "is_ffff( X1 ) &&"
  , "           " ++ prefix ++ "is_ffff( Y1 ) );"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_infinity ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "set_ffff( X3 );"
  , "  " ++ prefix ++ "set_ffff( Y3 );"
  , "}"
  , ""
  , "// checks the curve equation"
  , "//   y^2 == x^3 + A*x + B"
  , "uint8_t " ++ prefix ++ "is_on_curve ( const uint64_t *src1 ) {"
  , "  if (" ++ prefix ++ "is_infinity(src1)) {"
  , "    return 1;"
  , "  }"
  , "  else {"
  , "    uint64_t acc[" ++ show nlimbs_p ++ "];"
  , "    uint64_t tmp[" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "sqr( Y1, acc );             // Y^2"
  , "    " ++ prefix_p ++ "neg_inplace( acc );         // -Y^2"
  , "    " ++ prefix_p ++ "sqr( X1, tmp );             // X^2"
  , "    " ++ prefix_p ++ "mul_inplace( tmp, X1 );     // X^3"
  , "    " ++ prefix_p ++ "add_inplace( acc, tmp );    // - Y^2 + X^3"
  ] ++ 
  (if curveA == 0 then [] else 
    [ "    " ++ prefix   ++ "scale_by_A( X1, tmp );       // A*X"
    , "    " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2"
    ]
  ) ++ 
  (if curveB == 0 then [] else 
    [ "    " ++ prefix_p ++ "add_inplace( acc, " ++ prefix ++ "const_B );     // - Y^2*Z + X^3 + A*X + B"
    ]
  ) ++
  [ "    return " ++ prefix_p ++ "is_zero( acc );"
  , "  }"
  , "}"
  , ""
  , "// checks whether the given point is in the subgroup G1"
  , "uint8_t " ++ prefix ++ "is_in_subgroup ( const uint64_t *src1 ) {"
  , "  if (!" ++ prefix ++ "is_on_curve(src1)) {"
  , "    return 0;"
  , "  }"
  , "  else {"
  , "    if (" ++ prefix ++ "is_infinity(src1)) {"
  , "      return 1;"
  , "    }"
  , "    else {"
  , "      uint64_t proj[" ++ show (3*nlimbs_p) ++ "];"
  , "      uint64_t tmp [" ++ show (3*nlimbs_p) ++ "];"
  , "      " ++ prefix_proj ++ "from_affine( src1, proj );"
  , "      " ++ prefix_proj ++ "scl_Fr( " ++ prefix ++ "cofactor , proj , tmp );"
  , "      return " ++ prefix_proj ++ "is_infinity( tmp );"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  memcpy( tgt, src1, " ++ show (8*2*nlimbs_p) ++ " );"
  , "}"  
  ]

--------------------------------------------------------------------------------

negCurve :: Curve -> CodeGenParams -> Code
negCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// negates an elliptic curve point in affine coordinates" 
  , "void " ++ prefix ++ "neg( const uint64_t *src1, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity(src1)) {"
  , "    return;"
  , "  }"
  , "  else {"
  , "    memcpy( tgt, src1, " ++ show (8*nlimbs_p) ++ " );"
  , "    " ++ prefix_p ++ "neg( Y1, Y3 );"
  , "  }"
  , "}"
  , ""
  , "// negates an elliptic curve point in affine coordinates" 
  , "void " ++ prefix ++ "neg_inplace( uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity(tgt)) {"
  , "    return;"
  , "  }"
  , "  else {"
  , "    " ++ prefix_p ++ "neg_inplace( Y3 );"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

{-
  t  = (3*X1^2 + A) / (2*Y1)
  X3 = t^2 - 2*X1
  Y3 = - Y1 - t*(X3-X1)
-}

dblCurve :: Curve -> CodeGenParams -> Code
dblCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// doubles an affine elliptic curve point" 
  , "void " ++ prefix ++ "dbl( const uint64_t *src1, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity(src1)) {"
  , "    " ++ prefix ++ "copy( src1, tgt );"
  , "    return;"
  , "  }"
  , "  else {"
  , "    uint64_t  xx[" ++ show nlimbs_p ++ "];"
  , "    uint64_t   t[" ++ show nlimbs_p ++ "];"
  , "    uint64_t tmp[" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "sqr( X1 , xx );            // xx = X1^2"
  , "    " ++ prefix_p ++ "add( xx , xx, t );         // t  = 2*X1^2"
  , "    " ++ prefix_p ++ "add_inplace( t, xx );      // t  = 3*X1^2"
  ] ++ (if curveA == 0 then [] else 
    [ "    " ++ prefix_p ++ "add_inplace( t, " ++ prefix ++ "const_A );     // t = (3*X1^2 + A)"
    ]) ++
  [ "    " ++ prefix_p ++ "add( Y1, Y1, tmp );             // tmp = 2*Y1"
  , "    " ++ prefix_p ++ "div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)"
  , "    " ++ prefix_p ++ "sqr( t, tmp );                  // tmp = t^2"
  , "    " ++ prefix_p ++ "sub_inplace( tmp, X1 );         // tmp = t^2 - X1"
  , "    " ++ prefix_p ++ "sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1"
  , "    " ++ prefix_p ++ "sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1"
  , "    " ++ prefix_p ++ "mul_inplace( xx , t );          // xx = t*(X3 - X1)"
  , "    " ++ prefix_p ++ "add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)"
  , "    " ++ prefix_p ++ "copy( tmp, X3 );                // X3 = t^2 - 2*X1"
  , "    " ++ prefix_p ++ "neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)"
  , "  }"
  , "}"
  , ""
  , "// doubles an elliptic curve point, in place" 
  , "void " ++ prefix ++ "dbl_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "dbl( tgt , tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

{- 
  s  = (Y2 - Y1) / (X2 - X1)
  X3 = s^2 - X1 - X2
  Y3 = - Y1 - s*(X3 - X1)
-}

addCurve :: Curve -> CodeGenParams -> Code
addCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// adds two affine elliptic curve points" 
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  if (" ++ prefix ++ "is_infinity(src1)) {"
  , "    // PT1 = infinity"
  , "    " ++ prefix ++ "copy( src2, tgt );"
  , "    return;"
  , "  }"
  , "  if (" ++ prefix ++ "is_infinity(src2)) {"
  , "    // PT2 = infinity"
  , "    " ++ prefix ++ "copy( src1, tgt );"
  , "    return;"
  , "  }"
  , "  else {"
  , "    uint64_t xdif[" ++ show nlimbs_p ++ "];"
  , "    uint64_t  tmp[" ++ show nlimbs_p ++ "];"
  , "    uint64_t    s[" ++ show nlimbs_p ++ "];"
  , "    " ++ prefix_p ++ "sub( X2, X1, xdif );             // xdif = X2 - X1"
  , "    if (" ++ prefix_p ++ "is_zero(xdif)) {"
  , "      // X1 == X2"
  , "      if (" ++ prefix_p ++ "is_equal(Y1,Y2)) {"
  , "        // Y1 == Y2, it's a doubling"
  , "        " ++ prefix ++ "dbl( src1, tgt );"
  , "        return;"
  , "      }"
  , "      else {" 
  , "        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity"
  , "        " ++ prefix ++ "set_infinity(X3);"
  , "      }"
  , "    }"
  , "    else {" 
  , "      // normal addition"
  , "      " ++ prefix_p ++ "sub( Y2, Y1, s );             // s   = Y2 - Y1"
  , "      " ++ prefix_p ++ "div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)"
  , "      " ++ prefix_p ++ "sqr( s, tmp );                // tmp = s^2"
  , "      " ++ prefix_p ++ "sub_inplace( tmp, X1 );       // tmp = s^2 - X1"
  , "      " ++ prefix_p ++ "sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3"
  , "      " ++ prefix_p ++ "sub( tmp, X1 , xdif );        // xdif = X3 - X1"
  , "      " ++ prefix_p ++ "mul_inplace( xdif, s );       // xdif = s*(X3 - X1)"
  , "      " ++ prefix_p ++ "add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)"
  , "      " ++ prefix_p ++ "copy( tmp  , X3 );"
  , "      " ++ prefix_p ++ "neg ( xdif , Y3 );"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "add( tgt, src2, tgt);"
  , "}"
  ]

subCurve :: Curve -> CodeGenParams -> Code
subCurve (Curve{..}) (CodeGenParams{..}) =
  [ "void " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t tmp[" ++ show (2*nlimbs_p) ++ "];"
  , "  " ++ prefix ++ "neg( src2, tmp );"
  , "  " ++ prefix ++ "add( src1, tmp, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint64_t tmp[" ++ show (2*nlimbs_p) ++ "];"
  , "  " ++ prefix ++ "neg( src2, tmp );"
  , "  " ++ prefix ++ "add( tgt , tmp, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

scaleCurve :: Curve -> CodeGenParams -> Code
scaleCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr"
  , "void " ++ prefix ++ "scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {"
  , "  uint64_t proj1[3*NLIMBS_P];"
  , "  uint64_t proj2[3*NLIMBS_P];"
  , "  " ++ prefix_proj ++ "from_affine( grp, proj1 );"
  , "  " ++ prefix_proj ++ "scl_generic( expo, proj1, proj2, nlimbs);"
  , "  " ++ prefix_proj ++ "to_affine( proj2, tgt );"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr"
  , "void " ++ prefix ++ "scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t proj1[3*NLIMBS_P];"
  , "  uint64_t proj2[3*NLIMBS_P];"
  , "  " ++ prefix_proj ++ "from_affine( grp, proj1 );"
  , "  " ++ prefix_proj ++ "scl_Fr( expo, proj1, proj2);"
  , "  " ++ prefix_proj ++ "to_affine( proj2, tgt );"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is the same size as Fp"
  , "void " ++ prefix ++ "scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t proj1[3*NLIMBS_P];"
  , "  uint64_t proj2[3*NLIMBS_P];"
  , "  " ++ prefix_proj ++ "from_affine( grp, proj1 );"
  , "  " ++ prefix_proj ++ "scl_big( expo, proj1, proj2 );"
  , "  " ++ prefix_proj ++ "to_affine( proj2, tgt );"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word"
  , "void " ++ prefix ++ "scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  uint64_t proj1[3*NLIMBS_P];"
  , "  uint64_t proj2[3*NLIMBS_P];"
  , "  " ++ prefix_proj ++ "from_affine( grp, proj1 );"
  , "  " ++ prefix_proj ++ "scl_small( expo, proj1, proj2 );"
  , "  " ++ prefix_proj ++ "to_affine( proj2, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: Curve -> CodeGenParams -> Code
c_code curve params = concat $ map ("":)
  [ c_begin     curve params
    --
  , isOnCurve     curve params
    --
  , negCurve curve params
  , dblCurve curve params
  , addCurve curve params
  , subCurve curve params
    --
  , scaleCurve   curve params
  ]

hs_code :: Curve -> CodeGenParams -> Code
hs_code curve params@(CodeGenParams{..}) = concat $ map ("":)
  [ hsBegin      curve params
 -- , hsMiscTmp
 -- , hsConvert    curve params
  , hsFFI        curve params
  ]

--------------------------------------------------------------------------------

curve_MontAffine_c_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontAffine_c_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header curve params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code curve params

curve_MontAffine_hs_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontAffine_hs_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_hs   = tgtdir </> (hsFilePath hs_path)
  let fn_boot = fn_hs ++ "-boot"

  createTgtDirectory fn_hs
  
  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code curve params

  putStrLn $ "writing `" ++ fn_boot ++ "`" 
  writeFile fn_boot $ unlines $ hsBoot curve params

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

