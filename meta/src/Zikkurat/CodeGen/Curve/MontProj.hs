
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

--------------------------------------------------------------------------------

c_header :: Curve -> CodeGenParams -> Code
c_header (Curve{..}) (CodeGenParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "normalize         ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "normalize_inplace (       uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "copy     ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "from_aff ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "to_aff   ( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_on_curve   ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_infinity   ( const uint64_t *src );"
  , "extern void    " ++ prefix ++ "set_infinity  (       uint64_t *tgt );"
  , "extern uint8_t " ++ prefix ++ "is_in_subgroup( const uint64_t *src );"
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
  , "extern void " ++ prefix ++ "scl_Fp     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scl_Fr     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "scl_naive   ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  , "extern void " ++ prefix ++ "scl_windowed( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );"
  ]

--------------------------------------------------------------------------------

hsFFI :: Curve -> CodeGenParams -> Code
hsFFI (Curve{..}) (CodeGenParams{..}) = catCode $ 
  [ mkffi "isOnCurve"   $ cfun "is_on_curve"     (CTyp [CArgInProj] CRetBool)
  , mkffi "isInfinity"  $ cfun "is_infinity"     (CTyp [CArgInProj] CRetBool)
    --
  , mkffi "normalize"   $ cfun "normalize"        (CTyp [CArgInProj , CArgOutProj ] CRetVoid)
    --
-- TODO: we need an affine type first 
--  , mkffi "fromAffine"  $ cfun "from_aff"         (CTyp [CArgInAffine , CArgOutProj   ] CRetVoid)
--  , mkffi "toAffine"    $ cfun "to_aff"           (CTyp [CArgInProj   , CArgOutAffine ] CRetVoid)
    --
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInProj                 , CArgOutProj ] CRetVoid)
  , mkffi "dbl"         $ cfun "dbl"              (CTyp [CArgInProj                 , CArgOutProj ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInProj    , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInProj    , CArgInProj , CArgOutProj ] CRetVoid)
    --
  , mkffi "sclRef"      $ cfun "scl_reference"    (CTyp [CArgInScalarR , CArgInProj , CArgOutProj ] CRetVoid)
--  , mkffi "sclFp"       $ cfun "scl_Fp"           (CTyp [CArgInScalarP , CArgInProj , CArgOutProj ] CRetVoid)
  , mkffi "sclFr"       $ cfun "scl_Fr"           (CTyp [CArgInScalarR , CArgInProj , CArgOutProj ] CRetVoid)
  ]
  where
    -- cfun_ cname = CFun (bigint_   ++ cname)
    -- cfun' cname = CFun (stdPrefix ++ cname)
    cfun  cname = CFun (prefix    ++ cname)
    mkffi = curveFfiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeName
      , hsTyCon  = "Mk" ++ typeName
      , hsNLimbsP = nlimbs_p
      , hsNLimbsR = nlimbs_r
      }

--------------------------------------------------------------------------------

hsBegin :: Curve -> CodeGenParams -> Code
hsBegin (Curve{..}) (CodeGenParams{..}) =
  [ "-- NOTE 1: This module is intented to be imported qualified"
  , "-- NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}"
  , "module " ++ hsModule hs_path
  , "  ( " ++ typeName ++ "(..)"
  , "  , primeP , primeR , cofactor , curveA , curveB"
  , "  , genG1 , infinity"
  , "  , coords , mkPoint , mkPointMaybe , unsafeMkPoint"
  , "  , isOnCurve , isInfinity"
  , "  , normalize"
  , "  , neg , add , dbl , sub"
  , "  , sclRef , sclFr" -- , sclFp"
  , "  , rndG1 , rndG1_naive"
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
  , "-- import qualified ZK.Algebra.Class.Curve as C"
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
  , "genG1 = mkPoint (x, y, Fp.one) where"
  , "  x = " ++ show (fst subgroupGen)
  , "  y = " ++ show (snd subgroupGen)
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- | An elliptic curve point, in projective coordinates"
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
  , "-- | The point at infinity"
  , "infinity :: " ++ typeName
  , "infinity = unsafeMkPoint (Fp.zero, Fp.one, Fp.zero)"
  , ""
  , "{-# NOINLINE unsafeMkPoint #-}"
  , "unsafeMkPoint :: (Fp, Fp, Fp) -> " ++ typeName
  , "unsafeMkPoint (MkFp fptr1 , MkFp fptr2 , MkFp fptr3) = unsafePerformIO $ do"
  , "  fptr4 <- mallocForeignPtrArray " ++ show (3*nlimbs_p)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr2 $ \\ptr3 -> do"
  , "        withForeignPtr fptr2 $ \\ptr4 -> do"
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
  , "      withForeignPtr fptr2 $ \\ptr3 -> do"
  , "        withForeignPtr fptr2 $ \\ptr4 -> do"
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
  , ""
  , "#include \"" ++ pathBaseName c_path ++ ".h\""
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
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_A_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       ]

  1 -> [ "// scale a field element by A = " ++ show curveA
       , "void " ++ prefix ++ "scale_by_A(const uint64_t *src, uint64_t *tgt ) {"
       , "  memcpy( tgt, src, " ++ show (8*3*nlimbs_p) ++ " );"
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
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_B_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       ]

  1 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_B(const uint64_t *src, uint64_t *tgt ) {"
       , "  memcpy( tgt, src, " ++ show (8*3*nlimbs_p) ++ " );"
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

----------------------------------------

scale_by_3B ::  Curve -> CodeGenParams -> Code
scale_by_3B (Curve{..}) (CodeGenParams{..}) = case curveB of

  0 -> [ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
       , "}"
       ]

  3 -> [ "// scale a field element by B = " ++ show curveB
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

  4  ->[ "// scale a field element by B = " ++ show curveB
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  uint64_t tmp[NLIMBS_P];"
       , "  uint64_t tmp2[NLIMBS_P];"
       , "  " ++ prefix_p ++ "add( src, src, tmp );       // 2*B"
       , "  " ++ prefix_p ++ "add_inplace( tmp, tmp );    // 4*B"
       , "  " ++ prefix_p ++ "inplace( tmp, tmp, tmp2);   // 8*B"
       , "  " ++ prefix_p ++ "add( tmp, tmp2, tgt );      // 12*B"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix ++ "scale_by_3B( tgt , tgt );"
       , "}"
       ]

  _ -> [ "// scale a field element by 3*B = " ++ show (3*curveB)
       , "void " ++ prefix ++ "scale_by_3B(const uint64_t *src, uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "scl( " ++ show (3*curveB) ++ ", src, *tgt );"
       , "}"
       , ""
       , "void " ++ prefix ++ "scale_by_3B_inplace( uint64_t *tgt ) {"
       , "  " ++ prefix_p ++ "scl_inplace( tgt, " ++ show (3*curveB) ++ " );"
       , "}"
       ]

--------------------------------------------------------------------------------

normalize :: Curve -> CodeGenParams -> Code
normalize (Curve{..}) (CodeGenParams{..}) =
  [ "void " ++ prefix ++ "normalize( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t zinv[" ++ show nlimbs_p ++ "];"
  , "  if (" ++ prefix_p ++ "is_zero( Z1 ) ) {"
  , "    // Z == 0, it must be the point at infinity"
  , "    memset( tgt, 0, " ++ show (8*3*nlimbs_p) ++ " );"
  , "    " ++ prefix_p ++ "set_one( Y3 );"
  , "  }"
  , "  else {"
  , "    " ++ prefix_p ++ "inv( Z1, zinv );"
  , "    " ++ prefix_p ++ "mul( X1, zinv, X3 );"
  , "    " ++ prefix_p ++ "mul( Y1, zinv, Y3 );"
  , "    " ++ prefix_p ++ "set_one( Z3 );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "normalize_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "normalize( tgt, tgt );"
  , "}"
  ]

convertAffine :: Curve -> CodeGenParams -> Code
convertAffine (Curve{..}) (CodeGenParams{..}) = 
  [ "// converts from affine coordinates"
  , "void " ++ prefix ++ "from_aff( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  memcpy( tgt, src1, " ++ show (8*2*nlimbs_p) ++ " );"
  , "  " ++ prefix_p ++ "set_one( Z3 );"
  , "}"
  , ""
  , "// converts to affine coordinates"
  , "// remark: the point at infinity will result in (0,0)"
  , "void " ++ prefix ++ "to_aff( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  uint64_t zinv[" ++ show nlimbs_p ++ "];"
  , "  " ++ prefix_p ++ "inv( Z1, zinv );"
  , "  " ++ prefix_p ++ "mul( X1, zinv, X3 );"
  , "  " ++ prefix_p ++ "mul( Y1, zinv, Y3 );"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  memcpy( tgt, src1, " ++ show (8*3*nlimbs_p) ++ " );"
  , "}"  
  ]

isOnCurve :: Curve -> CodeGenParams -> Code
isOnCurve (Curve{..}) (CodeGenParams{..}) = 
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
  (if curveA == 0 then [] else 
    [ "  " ++ prefix_p ++ "mul( X1, ZZ, tmp );          // X*Z^2"
    , "  " ++ prefix   ++ "scale_by_A_inplace( tmp );   // A*X*Z^2"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2"
    ]
  ) ++ 
  (if curveB == 0 then [] else 
    [ "  " ++ prefix_p ++ "mul( Z1, ZZ, tmp );          // Z^3"
    , "  " ++ prefix   ++ "scale_by_B_inplace( tmp );   // B*Z^3"
    , "  " ++ prefix_p ++ "add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2 + B*Z^3"
    ]
  ) ++
  [ "  return " ++ prefix_p ++ "is_zero( acc );"
  , "}"
  , ""
  , "// checks whether the given point is in the subgroup G1"
  , "uint8_t " ++ prefix ++ "is_in_subgroup ( const uint64_t *src1 ) {"
  , "  uint64_t tmp[" ++ show (3*nlimbs_p) ++ "];"
  , "  if (!" ++ prefix ++ "is_on_curve(src1)) {"
  , "    return 0;"
  , "  }"
  , "  else {"
  , "    " ++ prefix ++ "scl_Fr( " ++ prefix ++ "cofactor , src1 , tmp );"
  , "    return " ++ prefix ++ "is_infinity( tmp );"
  , "  }"
  , "}"
  ]

-- TODO
-- subgroupCheck :: Curve -> CodeGenParams -> Code
-- subgroupCheck (Curve{..}) (CodeGenParams{..}) =

--------------------------------------------------------------------------------

negCurve :: Curve -> CodeGenParams -> Code
negCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// negates an elliptic curve point" 
  , "void " ++ prefix ++ "neg( const uint64_t *src, uint64_t *tgt ) {"
  , "  memcpy( tgt, src, " ++ show (8*3*nlimbs_p) ++ " );"
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
dblCurve :: Curve -> CodeGenParams -> Code
dblCurve (Curve{..}) (CodeGenParams{..}) =
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
  ] ++ (if curveA == 0 then [] else 
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

addCurve :: Curve -> CodeGenParams -> Code
addCurve (Curve{..}) (CodeGenParams{..}) =
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

mixedAddCurve :: Curve -> CodeGenParams -> Code
mixedAddCurve (Curve{..}) (CodeGenParams{..}) =
  [ "// adds a projective point (src1) to an affine point (src2)"
  , "// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-madd-1998-cmo"
  , "void " ++ prefix ++ "madd_proj_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
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
  , "void " ++ prefix ++ "scl_reference(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_naive(expo, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is in Fr"
  , "void " ++ prefix ++ "scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_windowed(expo, grp, tgt, NLIMBS_R);"
  , "}"
  , ""
  , "// computes `expo*grp` (or `grp^expo` in multiplicative notation)"
  , "// where `grp` is a group element in G1, and `expo` is the same size as Fp"
  , "void " ++ prefix ++ "scl_Fp(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {"
  , "  " ++ prefix ++ "scl_windowed(expo, grp, tgt, NLIMBS_P);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: Curve -> CodeGenParams -> Code
c_code curve params = concat $ map ("":)
  [ c_begin     curve params
    --
  , scale_by_A  curve params
  , scale_by_B  curve params
  , scale_by_3B curve params
    --
  , normalize     curve params
  , convertAffine curve params
  , isOnCurve     curve params
    --
  , negCurve curve params
  , dblCurve curve params
  , addCurve curve params
  , subCurve curve params
  , mixedAddCurve curve params
    --
  , scaleNaive    curve params
  , scaleWindowed curve params
  , scaleFpFr     curve params
  ]

hs_code :: Curve -> CodeGenParams -> Code
hs_code curve params@(CodeGenParams{..}) = concat $ map ("":)
  [ hsBegin      curve params
 -- , hsMiscTmp
 -- , hsConvert    curve params
  , hsFFI        curve params
  ]

--------------------------------------------------------------------------------

curve_MontProj_c_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontProj_c_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header curve params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code curve params

curve_MontProj_hs_codegen :: FilePath -> Curve -> CodeGenParams -> IO ()
curve_MontProj_hs_codegen tgtdir curve params@(CodeGenParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path)

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

