
-- | Pointwise operations on vectors of field elements etc

{-# LANGUAGE RecordWildCards, ExistentialQuantification, Rank2Types, StandaloneDeriving, ScopedTypeVariables #-}
module Zikkurat.CodeGen.Pointwise where

--------------------------------------------------------------------------------

import Data.Word
import Data.List
-- import Text.Printf

import System.FilePath

import Zikkurat.CodeGen.FFI
import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data PwParams = PwParams 
  { prefix           :: String         -- ^ prefix for C names
  , elem_prefix      :: String         -- ^ prefix for the C names of elements
  , elemNWords       :: Int            -- ^ size of the base-field elements, in number of 64-bit words 
  , c_path           :: Path           -- ^ path of the C module (without extension)
  , hs_path          :: Path           -- ^ path of the Hs module
  , c_path_base      :: Path           -- ^ C path of the base field
  , hs_path_base     :: Path           -- ^ the module path of the base field (montgomery repr)
  , hs_path_base_std :: Path           -- ^ the module path of the base field (standard repr)
  , typeNameBase     :: String         -- ^ the name of the haskell type of the base field (montgomery repr)
  , typeNameBaseStd  :: String         -- ^ the name of the haskell type of the base field (standard repr)
  }
  deriving Show

-- ^ the name of the haskell type
arrayTypeName :: PwParams -> String   
arrayTypeName (PwParams{..}) = "FlatArray " ++ typeNameBase

--------------------------------------------------------------------------------

c_header :: PwParams -> Code
c_header (PwParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_valid ( int n, const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_zero  ( int n, const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_equal ( int n, const uint64_t *src1, const uint64_t *src2 );"
  , "extern void    " ++ prefix ++ "set_zero ( int n,       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_one  ( int n,       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_const( int n, const uint64_t *src , uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "copy     ( int n, const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "from_std ( int n, const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "to_std   ( int n, const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg ( int n, const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sub ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sqr ( int n, const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "inv ( int n, const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg_inplace ( int n, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sub_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sqr_inplace ( int n, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul_inplace ( int n, uint64_t *tgt , const uint64_t *src2);"
  , "extern void " ++ prefix ++ "inv_inplace ( int n, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sub_inplace_reverse ( int n, uint64_t *tgt , const uint64_t *src1 );"
  , ""
  , "extern void " ++ prefix ++ "mul_add ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul_sub ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "dot_prod ( int n, const uint64_t *src1 , const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "powers ( int n, const uint64_t *coeffA , const uint64_t *coeffB, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "scale          ( int n, const uint64_t *coeff, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scale_inplace  ( int n, const uint64_t *coeff,       uint64_t *tgt   );"
  , ""
  , "extern void " ++ prefix ++ "Ax_plus_y         ( int n, const uint64_t *coeffA, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "Ax_plus_y_inplace ( int n, const uint64_t *coeffA,       uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "Ax_plus_By         ( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "Ax_plus_By_inplace ( int n, const uint64_t *coeffA, const uint64_t *coeffB,       uint64_t *tgt , const uint64_t *src2 );"
  , ""
  ]

--------------------------------------------------------------------------------

c_begin :: PwParams -> Code
c_begin params@(PwParams{..}) =
  [ "// vectors of `" ++ typeNameBase ++ "` elements"
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <string.h>"
  , "#include <stdlib.h>"
  , "#include <stdint.h>"
  , "#include <assert.h>"
  , ""
  , "#include \"" ++ pathBaseName c_path      ++ ".h\""
  , "#include \"" ++ pathBaseName c_path_base ++ ".h\""
  , ""
  , "#define ELEM_NWORDS " ++ show elemNWords
  , ""
  , "#define SRC1(i) ((src1) + (i)*ELEM_NWORDS)"
  , "#define SRC2(i) ((src2) + (i)*ELEM_NWORDS)"
  , "#define SRC3(i) ((src3) + (i)*ELEM_NWORDS)"
  , "#define TGT(i)  (( tgt) + (i)*ELEM_NWORDS)"
  , "#define TMP(i)  (( tmp) + (i)*ELEM_NWORDS)"
  , ""
  , "//------------------------------------------------------------------------------"
  , ""
  ]

--------------------------------------------------------------------------------

c_zero_one_etc :: PwParams -> Code
c_zero_one_etc params@(PwParams{..}) =
  [ "uint8_t " ++ prefix ++ "is_valid ( int n, const uint64_t *src1 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int i=0; i<n; i++) {"
  , "    if (!" ++ elem_prefix ++ "is_valid( SRC1(i) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_zero  ( int n, const uint64_t *src1 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int i=0; i<n; i++) {"
  , "    if (!" ++ elem_prefix ++ "is_zero( SRC1(i) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_equal ( int n, const uint64_t *src1, const uint64_t *src2 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int i=0; i<n; i++) {"
  , "    if (!" ++ elem_prefix ++ "is_equal( SRC1(i) , SRC2(i) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_zero ( int n, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "set_zero( TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "set_one  ( int n, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "set_one( TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "set_const( int n, const uint64_t *src , uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "copy( src , TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "copy ( int n, const uint64_t *src , uint64_t *tgt ) {"
  , "  memcpy( tgt, src, (8*ELEM_NWORDS)*n );"
  , "}"
  , ""
  ]

--------------------------------------------------------------------------------

c_convert :: PwParams -> Code
c_convert params@(PwParams{..}) =
  [ "void " ++ prefix ++ "from_std ( int n, const uint64_t *src1 , uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "from_std ( SRC1(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "to_std ( int n, const uint64_t *src1 , uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "to_std ( SRC1(i), TGT(i) ); "
  , "}"
  , ""
  ]

--------------------------------------------------------------------------------

c_add_sub_mul_etc :: PwParams -> Code
c_add_sub_mul_etc params@(PwParams{..}) =
  [ "void " ++ prefix ++ "neg ( int n, const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "neg( SRC1(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "add ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "add( SRC1(i), SRC2(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "sub ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "sub( SRC1(i), SRC2(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "sqr ( int n, const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "sqr( SRC1(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "mul ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul( SRC1(i), SRC2(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "inv ( int n, const uint64_t *src1, uint64_t *tgt ) {"
  , "  " ++ elem_prefix ++ "batch_inv( n, src1, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "div ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t *tmp = (uint64_t*) malloc( n*(8*ELEM_NWORDS) );"
  , "  assert( tmp != 0);"
  , "  " ++ elem_prefix ++ "batch_inv( n, src2, tmp );"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul( SRC1(i), TMP(i), TGT(i) ); "
  , "  free(tmp);"
  , "}"
  , ""
  , "// computes the vector `A*B+C`"
  , "void " ++ prefix ++ "mul_add ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) {"
  , "    " ++ elem_prefix ++ "mul( SRC1(i), SRC2(i), TGT(i) ); "
  , "    " ++ elem_prefix ++ "add_inplace( TGT(i) , SRC3(i) ); "
  , "  }"
  , "}"
  , ""
  , "// computes the vector `A*B-C`"
  , "void " ++ prefix ++ "mul_sub ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) {"
  , "    " ++ elem_prefix ++ "mul( SRC1(i), SRC2(i), TGT(i) ); "
  , "    " ++ elem_prefix ++ "sub_inplace( TGT(i) , SRC3(i) ); "
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

c_add_sub_mul_etc_inplace :: PwParams -> Code
c_add_sub_mul_etc_inplace params@(PwParams{..}) =
  [ "void " ++ prefix ++ "neg_inplace ( int n, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "neg_inplace( TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "add_inplace( TGT(i) , SRC2(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "sub_inplace( TGT(i) , SRC2(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace_reverse ( int n, uint64_t *tgt , const uint64_t *src1 ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "sub_inplace_reverse( TGT(i) , SRC1(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "sqr_inplace ( int n, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "sqr_inplace( TGT(i) );"
  , "}"
  , ""
  , "void " ++ prefix ++ "mul_inplace ( int n, uint64_t *tgt , const uint64_t *src2) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul_inplace( TGT(i) , SRC2(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "inv_inplace ( int n, uint64_t *tgt ) {"
  , "  " ++ elem_prefix ++ "batch_inv( n, tgt, tgt );   // batch_inv is inplace-safe"
  , "}"
  , ""
  , "void " ++ prefix ++ "div_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {"
  , "  uint64_t *tmp = malloc( n*(8*ELEM_NWORDS) );"
  , "  " ++ elem_prefix ++ "batch_inv( n, src2, tmp );"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul_inplace( TGT(i) , TMP(i) ); "
  , "  free(tmp);"
  , "}"
  , ""
  ]

--------------------------------------------------------------------------------

c_dotprod_etc :: PwParams -> Code
c_dotprod_etc params@(PwParams{..}) =
  [ "void " ++ prefix ++ "dot_prod  ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t tmp[ELEM_NWORDS];"
  , "  uint64_t acc[ELEM_NWORDS];"
  , "  " ++ elem_prefix ++ "set_zero( acc );"
  , "  for(int i=0; i<n; i++) {"
  , "    " ++ elem_prefix ++ "mul( SRC1(i) , SRC2(i) , tmp );"
  , "    " ++ elem_prefix ++ "add_inplace( acc, tmp );"
  , "  }"  
  , "  " ++ elem_prefix ++ "copy( acc, tgt );"
  , "}"
  , ""
  , "// generate the vector `[ a*b^i | i<-[0..n-1] ]`"
  , "void " ++ prefix ++ "powers ( int n, const uint64_t *coeffA, const uint64_t *coeffB, uint64_t *tgt ) {"
  , "  if (n==0) return;"
  , "  " ++ elem_prefix ++ "copy( coeffA, TGT(0) );"
  , "  for(int i=1; i<n; i++) {"
  , "  " ++ elem_prefix ++ "mul( TGT(i-1), coeffB, TGT(i) );"
  , "  }"  
  , "}"
  ]

--------------------------------------------------------------------------------

c_scale_lincomb_etc :: PwParams -> Code
c_scale_lincomb_etc params@(PwParams{..}) =
  [ ""
  , "void " ++ prefix ++ "scale ( int n, const uint64_t *coeff, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul( coeff, SRC2(i), TGT(i) ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "scale_inplace  ( int n, const uint64_t *coeff, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) " ++ elem_prefix ++ "mul_inplace( TGT(i) , coeff ); "
  , "}"
  , ""
  , "void " ++ prefix ++ "Ax_plus_y ( int n, const uint64_t *coeffA, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int i=0; i<n; i++) {" 
  , "    " ++ elem_prefix ++ "mul( coeffA, SRC1(i), TGT(i) ); "
  , "    " ++ elem_prefix ++ "add_inplace( TGT(i) , SRC2(i) ); "
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "Ax_plus_y_inplace ( int n, const uint64_t *coeffA, uint64_t *tgt , const uint64_t *src2 ) {"
  , "  for(int i=0; i<n; i++) {" 
  , "    " ++ elem_prefix ++ "mul_inplace( TGT(i) , coeffA  ); "
  , "    " ++ elem_prefix ++ "add_inplace( TGT(i) , SRC2(i) ); "
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "Ax_plus_By ( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t tmp1[ELEM_NWORDS];"
  , "  uint64_t tmp2[ELEM_NWORDS];"
  , "  for(int i=0; i<n; i++) {" 
  , "    " ++ elem_prefix ++ "mul( coeffA , SRC1(i) , tmp1 );"
  , "    " ++ elem_prefix ++ "mul( coeffB , SRC2(i) , tmp2 );"
  , "    " ++ elem_prefix ++ "add( tmp1, tmp2, TGT(i) ); "
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "Ax_plus_By_inplace ( int n, const uint64_t *coeffA, const uint64_t *coeffB, uint64_t *tgt , const uint64_t *src2 ) {"
  , "  uint64_t tmp[ELEM_NWORDS];"
  , "  for(int i=0; i<n; i++) {" 
  , "    " ++ elem_prefix ++ "mul_inplace( TGT(i), coeffA  );"
  , "    " ++ elem_prefix ++ "mul( coeffB , SRC2(i) , tmp );"
  , "    " ++ elem_prefix ++ "add_inplace( TGT(i) , tmp );"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

hsBegin :: PwParams -> Code
hsBegin (PwParams{..}) =
  [ "-- | Arrays over '" ++ hsModule hs_path_base ++ "." ++ typeNameBase ++ "'"
  , "--"
  , "-- * NOTE 1: This module is intented to be imported qualified"
  , "--"
  , "-- * NOTE 2: Generated code, do not edit!"
  , "--"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface, PatternSynonyms, TypeFamilies, FlexibleInstances #-}"
  , "module " ++ hsModule hs_path
  , "  ( -- * Boolean predicates on arrays"
  , "    isValid"
  , "  , isZero , isOne"  
  , "  , isEqual"
  , "    -- * Array conversion"
  , "  , fromStd , toStd"
  , "    -- * Pointwise arithmetics"
  , "  , neg , add , sub"  
  , "  , sqr , mul"  
  , "  , inv , div"   
  , "    -- * Misc"
  , "  , scale"
  , "  , dotProd"
  , "  , powers" 
  , "    -- * Fused mul-add"
  , "  , mulAdd"
  , "  , mulSub"
  , "    -- * Linear combination"
  , "  , linComb1"
  , "  , linComb2"
  , "  )"  
  , "  where"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "import Prelude  hiding (div,quot,rem)"
  , "import GHC.Real hiding (div,quot,rem)"
  , ""
  , "import Data.Bits"
  , "import Data.Word"
  , "import Data.List"
  , "import Data.Array"
  , ""
  , "import Control.Monad"
  , ""
  , "import Foreign.C"
  , "import Foreign.Ptr"
  , "import Foreign.Marshal"
  , "import Foreign.ForeignPtr"
  , ""
  , "import System.Random"
  , "import System.IO.Unsafe"
  , ""
  , "import " ++ hsModule hs_path_base ++ " ( " ++ typeNameBase ++ "(..) )"
  , "import qualified " ++ hsModule hs_path_base
  , "import qualified " ++ hsModule hs_path_base_std ++ " as Std"
  , ""
  , "import ZK.Algebra.Class.Flat ( FlatArray(..) )"
  , ""
  , "import           ZK.Algebra.Class.Flat   as L"
  , "import           ZK.Algebra.Class.FFT    as T"
  , "import qualified ZK.Algebra.Class.Field  as F"
  , "import qualified ZK.Algebra.Class.Poly   as P"
  , "import qualified ZK.Algebra.Class.Vector as V"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "instance Eq (FlatArray " ++ typeNameBase ++ ") where"
  , "  arr1 == arr2"
  , "    | flatArrayLength arr1 == flatArrayLength arr2  = isEqual arr1 arr2"
  , "    | otherwise                                     = False"
  , ""
  , "instance V.PointwiseGroup (FlatArray " ++ typeNameBase ++ ") where"
  , "  pwNeg = neg"
  , "  pwAdd = add"
  , "  pwSub = sub"
  , ""
  , "instance V.PointwiseRing (FlatArray " ++ typeNameBase ++ ") where"
  , "  pwSqr    = sqr"
  , "  pwMul    = mul"
  , "  pwMulAdd = mulAdd"
  , "  pwMulSub = mulSub"
  , ""
  , "instance V.PointwiseField (FlatArray " ++ typeNameBase ++ ") where"
  , "  pwInv = " ++ hsModule hs_path ++ ".inv"
  , "  pwDiv = " ++ hsModule hs_path ++ ".div"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "instance V.VectorSpace (FlatArray " ++ typeNameBase ++ ") where"
  , "  -- type Element (FlatArray " ++ typeNameBase ++ ") = " ++ typeNameBase
  , "  vecSize  = flatArrayLength"
  , "  vecIndex = flip peekFlatArray"
  , "  vecScale = " ++ hsModule hs_path ++ ".scale"
  , "  dotProd  = " ++ hsModule hs_path ++ ".dotProd"
  , "  powers !a !b !n = " ++ hsModule hs_path ++ ".powers n a b"
  , "  linComb1 = " ++ hsModule hs_path ++ ".linComb1"
  , "  linComb2 = " ++ hsModule hs_path ++ ".linComb2"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- void " ++ prefix ++ "Ax_plus_y ( int n, const uint64_t *coeffA,                         const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "-- void " ++ prefix ++ "Ax_plus_By( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "Ax_plus_y\"  c_" ++ prefix ++ "Ax_plus_y  :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ prefix ++ "Ax_plus_By\" c_" ++ prefix ++ "Ax_plus_By :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE linComb1 #-}"
  , "linComb1 :: (" ++ typeNameBase ++ ", FlatArray " ++ typeNameBase ++ ") -> FlatArray " ++ typeNameBase ++ " -> FlatArray " ++ typeNameBase
  , "linComb1 (Mk" ++ typeNameBase ++ " fptr_a, MkFlatArray n1 fptr_x) (MkFlatArray n2 fptr_y)"
  , "  | n1 /= n2   = error \"linComb1: incompatible vector dimensions\""
  , "  | otherwise  = unsafePerformIO $ do"
  , "      fptr_o <- mallocForeignPtrArray (n1*" ++ show elemNWords ++ ")"
  , "      withForeignPtr fptr_a $ \\ptr_a -> do"
  , "        withForeignPtr fptr_x $ \\ptr_x -> do"
  , "          withForeignPtr fptr_y $ \\ptr_y -> do"
  , "            withForeignPtr fptr_o $ \\ptr_o -> do" 
  , "              c_" ++ prefix ++ "Ax_plus_y (fromIntegral n1) ptr_a ptr_x ptr_y ptr_o"
  , "      return (MkFlatArray n1 fptr_o)"
  , ""
  , "{-# NOINLINE linComb2 #-}"
  , "linComb2 :: (" ++ typeNameBase ++ ", FlatArray " ++ typeNameBase ++ ") -> (" ++ typeNameBase ++ ", FlatArray " ++ typeNameBase ++ ") -> FlatArray " ++ typeNameBase
  , "linComb2 (Mk" ++ typeNameBase ++ " fptr_a, MkFlatArray n1 fptr_x) (Mk" ++ typeNameBase ++ " fptr_b, MkFlatArray n2 fptr_y)"
  , "  | n1 /= n2   = error \"linComb2: incompatible vector dimensions\""
  , "  | otherwise  = unsafePerformIO $ do"
  , "      fptr_o <- mallocForeignPtrArray (n1*" ++ show elemNWords ++ ")"
  , "      withForeignPtr fptr_a $ \\ptr_a -> do"
  , "        withForeignPtr fptr_b $ \\ptr_b -> do"
  , "          withForeignPtr fptr_x $ \\ptr_x -> do"
  , "            withForeignPtr fptr_y $ \\ptr_y -> do"
  , "              withForeignPtr fptr_o $ \\ptr_o -> do" 
  , "                c_" ++ prefix ++ "Ax_plus_By (fromIntegral n1) ptr_a ptr_b ptr_x ptr_y ptr_o"
  , "      return (MkFlatArray n1 fptr_o)"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  ]

--------------------------------------------------------------------------------

hsFFI :: PwParams -> Code
hsFFI (PwParams{..}) = catCode $ 
  [ mkffi "isValid"     $ cfun "is_valid"     (CTyp [CArgCount, CArgInArrPtr                ] CRetBool)
  , mkffi "isZero"      $ cfun "is_zero"      (CTyp [CArgCount, CArgInArrPtr                ] CRetBool)
  , mkffi "isOne"       $ cfun "is_one"       (CTyp [CArgCount, CArgInArrPtr                ] CRetBool)
  , mkffi "isEqual"     $ cfun "is_equal"     (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr ] CRetBool)
    --    --
  , mkffi "neg"         $ cfun "neg"          (CTyp [CArgCount, CArgInArrPtr                , CArgOutArrPtr ] CRetVoid)
  , mkffi "add"         $ cfun "add"          (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"          (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
  , mkffi "sqr"         $ cfun "sqr"          (CTyp [CArgCount, CArgInArrPtr                , CArgOutArrPtr ] CRetVoid)
  , mkffi "mul"         $ cfun "mul"          (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
  , mkffi "inv"         $ cfun "inv"          (CTyp [CArgCount, CArgInArrPtr                , CArgOutArrPtr ] CRetVoid)
  , mkffi "div"         $ cfun "div"          (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
    --
  , mkffi "dotProd"     $ cfun "dot_prod"     (CTyp [CArgCount, CArgInArrPtr, CArgInArrPtr, CArgOutPtr ] CRetVoid)
  , mkffi "powers"      $ cfun "powers"       (CTyp [CArgCount, CArgInPtr , CArgInPtr , CArgOutArrPtr ] CRetVoid)
    --
  , mkffi "mulAdd"      $ cfun "mul_add"      (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
  , mkffi "mulSub"      $ cfun "mul_sub"      (CTyp [CArgCount, CArgInArrPtr , CArgInArrPtr , CArgInArrPtr , CArgOutArrPtr ] CRetVoid)
    --
  , mkffi "scale"       $ cfun "scale"        (CTyp [CArgCount, CArgInPtr, CArgInArrPtr, CArgOutArrPtr ] CRetVoid)
  ]
  where
    cfun  cname = CFun (prefix      ++ cname)
    mkffi = ffiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeNameBase
      , hsTyCon  = "Mk" ++ typeNameBase
      , hsNLimbs = elemNWords
      }

{-
TODO:
  , "extern void " ++ prefix ++ "Ax_plus_y         ( int n, const uint64_t *coeffA, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "Ax_plus_y_inplace ( int n, const uint64_t *coeffA,       uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "Ax_plus_By         ( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "Ax_plus_By_inplace ( int n, const uint64_t *coeffA, const uint64_t *coeffB,       uint64_t *tgt , const uint64_t *src2 );"
-}

--------------------------------------------------------------------------------

hsConvert:: PwParams -> Code
hsConvert (PwParams{..}) = 
  [ "foreign import ccall unsafe \"" ++ prefix ++ "from_std" ++ "\" c_" ++ prefix ++ "from_std :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ prefix ++ "to_std"   ++ "\" c_" ++ prefix ++ "to_std   :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE fromStd #-}"
  , "fromStd :: FlatArray " ++ typeNameBaseStd ++ " -> FlatArray " ++ typeNameBase
  , "fromStd (MkFlatArray n1 fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray (n1*" ++ show elemNWords ++ ")"
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "from_std (fromIntegral n1) ptr1 ptr2"
  , "  return (MkFlatArray n1 fptr2)"
  , ""
  , "{-# NOINLINE toStd #-}"
  , "toStd :: FlatArray " ++ typeNameBase ++ " -> FlatArray " ++ typeNameBaseStd
  , "toStd (MkFlatArray n1 fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray (n1*" ++ show elemNWords ++ ")"
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "to_std (fromIntegral n1) ptr1 ptr2"
  , "  return (MkFlatArray n1 fptr2)"
  ]

--------------------------------------------------------------------------------

c_code :: PwParams -> Code
c_code pwparams = concat $ map ("":)
  [ c_begin                   pwparams
  , c_zero_one_etc            pwparams
  , c_convert                 pwparams
  , c_add_sub_mul_etc         pwparams
  , c_add_sub_mul_etc_inplace pwparams
  , c_dotprod_etc             pwparams
  , c_scale_lincomb_etc       pwparams
  ]

hs_code :: PwParams -> Code
hs_code pwparams@(PwParams{..}) = concat $ map ("":)
  [ hsBegin      pwparams
  , hsConvert    pwparams
  , hsFFI        pwparams
  ]

--------------------------------------------------------------------------------

pw_array_c_codegen :: FilePath -> PwParams -> IO ()
pw_array_c_codegen tgtdir params@(PwParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code params

pw_array_hs_codegen :: FilePath -> PwParams -> IO ()
pw_array_hs_codegen tgtdir params@(PwParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code params

--------------------------------------------------------------------------------

