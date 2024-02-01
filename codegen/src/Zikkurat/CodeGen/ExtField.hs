
-- | Field extensions (quadratic, cubic, generic)

{-# LANGUAGE RecordWildCards, ExistentialQuantification, Rank2Types, StandaloneDeriving, ScopedTypeVariables #-}
module Zikkurat.CodeGen.ExtField where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.List
import Data.Proxy
import Text.Printf

import System.FilePath

import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Misc

import Zikkurat.CodeGen.FieldCommon as Common
import Zikkurat.CodeGen.FFI
import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

isMinusOne :: Field f => f -> Bool
isMinusOne  x = isZero (x + 1)

--------------------------------------------------------------------------------

data AnyExtProxy 
  = forall f. (Field f, SerializeMontgomery f, ExtField' f) => AnyExtProxy (Proxy f)

deriving instance Show AnyExtProxy

--------------------------------------------------------------------------------

-- | Coefficients in little-endian order. We assume monic polynomial, 
-- and may skip the leading coefficient 1 
newtype IrredPoly f 
  = IrredPoly [f] 
  deriving Show

data AnyIrredPoly 
  = forall f. Field f => AnyIrredPoly (IrredPoly f)

deriving instance Show AnyIrredPoly

-- | NOTE: we want Montgomery representation here!
irredPolyToCString :: SerializeMontgomery f => String -> IrredPoly f -> String
irredPolyToCString prefix (IrredPoly coeffs) = 
  wordsToCHexString (prefix ++ "irred_coeffs") (concatMap toWordsMontgomery $ coeffs)

anyIrredPolyToCString :: String -> AnyIrredPoly -> String
anyIrredPolyToCString prefix any = case any of
  AnyIrredPoly irred -> irredPolyToCString prefix irred

withIrredCoeffs :: AnyIrredPoly -> (forall f. Field f => [f] -> a) -> a
withIrredCoeffs any fun = case any of
  AnyIrredPoly (IrredPoly coeffs) -> fun coeffs

--------------------------------------------------------------------------------

data ExtParams = ExtParams 
  { prefix         :: String         -- ^ prefix for C names
  , prime_prefix   :: String         -- ^ perfix for the C names of the prime field
  , base_prefix    :: String         -- ^ perfix for the C names of the base field
  , primeDegree    :: Int            -- ^ degree of the extension over the prime field
  , extDegree      :: Int            -- ^ degree of the extension over the base field
  , primeNWords    :: Int            -- ^ size of the primefield elements, in number of 64-bit words 
  , baseNWords     :: Int            -- ^ size of the base-field elements, in number of 64-bit words 
  , c_path         :: Path           -- ^ path of the C module (without extension)
  , c_path_base    :: Path           -- ^ C path of the base field
  , c_path_prime   :: Path           -- ^ C path of the base prime
  , hs_path        :: Path           -- ^ path of the Hs module
  , hs_path_base   :: Path           -- ^ the module path of the base field
  , hs_path_prime  :: Path           -- ^ the module path of the prime field
  , typeName       :: String         -- ^ the name of the haskell type
  , typeNameBase   :: String         -- ^ the name of the haskell type of the base field
  , typeNamePrime  :: String         -- ^ the name of the haskell type of the prime field
  , extFieldName   :: String         -- ^ name of the field
  , irredPoly      :: AnyIrredPoly   -- ^ the defining polynomial
  , pureTypeProxy  :: AnyExtProxy    -- ^ the defining polynomial
  , expoBigintType :: String         -- ^ bigint type used for the exponent in @pow@
--  , primGen       :: Integer       -- ^ the primitive generator
  }
  deriving Show

extNWords :: ExtParams -> Int
extNWords ep = extDegree ep * baseNWords ep

toCommonParams :: ExtParams -> CommonParams
toCommonParams (ExtParams{..}) = CommonParams 
  { Common.prefix     = prefix
  , Common.nlimbs     = extDegree * baseNWords
  , Common.typeName   = typeName
  , Common.bigintType = expoBigintType -- error "ExtField: CommonParams: bigintType"
  }

--------------------------------------------------------------------------------

c_header :: ExtParams -> Code
c_header (ExtParams{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "from_base_field ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "from_prime_field( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "scale_by_base_field ( const uint64_t *coeff , const uint64_t *src, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "scale_by_prime_field( const uint64_t *coeff , const uint64_t *src, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "frobenius( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "frobenius_inplace( uint64_t *tgt );"
  ,""
  , "extern uint8_t " ++ prefix ++ "is_valid ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_zero  ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_one   ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_equal ( const uint64_t *src1, const uint64_t *src2 );"
  , "extern void    " ++ prefix ++ "set_zero (       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_one  (       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_const( const uint64_t *src , uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "copy     ( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sqr ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "inv ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg_inplace ( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sub_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sqr_inplace ( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul_inplace ( uint64_t *tgt , const uint64_t *src2);"
  , "extern void " ++ prefix ++ "inv_inplace ( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 );"
  , ""
  , "extern void " ++ prefix ++ "div_by_2         ( const uint64_t *src1 , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_by_2_inplace ( uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "batch_inv( int n, const uint64_t *src, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );"
  ]

--------------------------------------------------------------------------------

c_begin :: ExtParams -> Code
c_begin params@(ExtParams{..}) =
  [ "// extension field `" ++ extFieldName ++ "`"
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
  , (if pathBaseName c_path_prime == pathBaseName c_path_base then "" else 
     "#include \"" ++ pathBaseName c_path_prime ++ ".h\"")
  , ""
  , "#define PRIME_NWORDS " ++ show primeNWords
  , "#define BASE_NWORDS  " ++ show baseNWords
  , "#define EXT_NWORDS   " ++ show (baseNWords * extDegree)
  , ""
  , "#define EXT_DEGREE   " ++ show extDegree
  , "#define PRIME_DEGREE " ++ show primeDegree
  , ""
  , "#define SRC1(i) ((src1) + (i)*BASE_NWORDS)"
  , "#define SRC2(i) ((src2) + (i)*BASE_NWORDS)"
  , "#define TGT(i)  (( tgt) + (i)*BASE_NWORDS)"
  , "#define PROD(i) ((prod) + (i)*BASE_NWORDS)"
  , "#define IRRED(i) (" ++ prefix ++ "irred_coeffs + (i)*BASE_NWORDS)"
  , ""
  , "#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )"
  , "#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )"
  , ""
  , anyIrredPolyToCString prefix irredPoly
  , ""
  , "//------------------------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------

c_basefield :: ExtParams -> Code
c_basefield ExtParams{..} = 
  [ ""
  , "void " ++ prefix ++ "from_base_field ( const uint64_t *src , uint64_t *tgt ) {"
  , "  " ++ prefix      ++ "set_zero( tgt );"
  , "  " ++ base_prefix ++ "copy( src, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "from_prime_field( const uint64_t *src , uint64_t *tgt ) {"
  , "  " ++ prefix       ++ "set_zero( tgt );"
  , "  " ++ prime_prefix ++ "copy( src, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "scale_by_base_field ( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int i=0; i<EXT_DEGREE; i++) {"
  , "    " ++ base_prefix ++ "mul( coeff, SRC1(i), TGT(i) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "scale_by_prime_field( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int i=0; i<PRIME_DEGREE; i++) {"
  , "    " ++ prime_prefix ++ "mul( coeff , src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "div_by_2( const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int i=0; i<PRIME_DEGREE; i++) {"
  , "    " ++ prime_prefix ++ "div_by_2( src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "div_by_2_inplace(  uint64_t *tgt ) {"
  , "  for(int i=0; i<PRIME_DEGREE; i++) {"
  , "    " ++ prime_prefix ++ "div_by_2_inplace( tgt + i*PRIME_NWORDS );"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

c_zeroOneEtcExt :: ExtParams -> Code
c_zeroOneEtcExt ExtParams{..} =
  [ "uint8_t " ++ prefix ++ "is_valid ( const uint64_t *src1 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    if (!" ++ base_prefix ++ "is_valid( SRC1(k) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_zero  ( const uint64_t *src1 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    if (!" ++ base_prefix ++ "is_zero( SRC1(k) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_one ( const uint64_t *src1 ) {"
  , "  uint8_t ok = " ++ base_prefix ++ "is_one( SRC1(0) );"
  , "  if (!ok) { return ok; }"
  , "  for(int k=1; k<EXT_DEGREE; k++) {"
  , "    if (!" ++ base_prefix ++ "is_zero( SRC1(k) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_equal ( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  uint8_t ok = 1;"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    if (!" ++ base_prefix ++ "is_equal( SRC1(k) , SRC2(k) )) { ok=0; break; }"
  , "  }"
  , "  return ok;"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_zero ( uint64_t *tgt ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "set_zero( TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_one ( uint64_t *tgt ) {"
  , "  " ++ base_prefix ++ "set_one( tgt );"
  , "  for(int k=1; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "set_zero( TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "// src: base field"
  , "// tgt: extension field"
  , "void " ++ prefix ++ "set_const ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  " ++ base_prefix ++ "copy( src1, tgt );"
  , "  for(int k=1; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "set_zero( TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy ( const uint64_t *src1 , uint64_t *tgt ) {"
  , "  memcpy( tgt, src1, 8*EXT_NWORDS );"
  , "  // for(int k=0; k<EXT_DEGREE; k++) {"
  , "  //   " ++ base_prefix ++ "copy( SRC1(k) , TGT(k) );"
  , "  // }"
  , "}"
  , ""
  ]

--------------------------------------------------------------------------------

c_addSubExt :: ExtParams -> Code
c_addSubExt ExtParams{..} = 
  [ "void " ++ prefix ++ "neg ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "neg( SRC1(k) , TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "add( SRC1(k) , SRC2(k) , TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "sub( SRC1(k) , SRC2(k) , TGT(k) );"
  , "  }"
  , "}"
  , ""
  , "//--------------------------------------"
  , ""
  , "void " ++ prefix ++ "neg_inplace ( uint64_t *tgt ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "neg_inplace( TGT(k) );"
  , "  }"
  , "}"
  , "void " ++ prefix ++ "add_inplace ( uint64_t *tgt , const uint64_t *src2 ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "add_inplace( TGT(k) , SRC2(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace ( uint64_t *tgt , const uint64_t *src2 ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "sub_inplace( TGT(k) , SRC2(k) );"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 ) {"
  , "  for(int k=0; k<EXT_DEGREE; k++) {"
  , "    " ++ base_prefix ++ "sub_inplace_reverse( TGT(k) , SRC1(k) );"
  , "  }"
  , "}"
  , ""
  ]

--------------------------------------------------------------------------------

c_mulExt :: ExtParams -> Code
c_mulExt extparams@(ExtParams{..}) 
 | extDegree <= 1   = error "fatal error: field extension degree must be > 1"
 | extDegree == 2   =                                     c_mulExtQuadratic  extparams ++ c_mulExtInplace extparams
 | extDegree == 3   = c_subtractIrredGeneric extparams ++ c_mulExtCubic      extparams ++ c_mulExtInplace extparams
 | otherwise        = c_subtractIrredGeneric extparams ++ c_mulExtGeneric    extparams ++ c_mulExtInplace extparams
--  | extDegree <= 8   = mulExtGenericSmall params
--  | otherwise        =  error ("multiplication is not yet implemented for field extension of degree " ++ show extDegree)

----------------------------------------

{-
-- "small" because we inline the subtraction
c_mulExtGenericSmall :: ExtParams -> Code
c_mulExtGenericSmall ExtParams{..} =  
  [ "void " ++ prefix ++ "subtract_irred_small( const uint64_t *scalar, uint64_t *tgt ) {"
  , "  uint64_t tmp[BASE_NWORDS];" 
  , "  for(int k=0; k<" ++ show extDegree ++ "; k++) {"
  , "    
  , "  }"
  , "}"
  , ""
-}

c_subtractIrredGeneric :: ExtParams -> Code
c_subtractIrredGeneric ExtParams{..} =  
  [ "void " ++ prefix ++ "subtract_irred_generic( const uint64_t *scalar, uint64_t *tgt ) {"
  , "  for(int k=0; k<" ++ show extDegree ++ "; k++) {"
  , "    uint64_t tmp[BASE_NWORDS];" 
  , "    " ++ base_prefix ++ "mul( scalar , IRRED(k) , tmp );" 
  , "    " ++ base_prefix ++ "sub_inplace( TGT(k) , tmp );" 
  , "  }"
  , "}"
  , ""
  ]

c_mulExtGeneric :: ExtParams -> Code
c_mulExtGeneric ExtParams{..} =  
  [ "void " ++ prefix ++ "mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t prod[" ++ show (2*extDegree-1) ++ "*BASE_NWORDS];" 
  , "  uint64_t tmp[BASE_NWORDS];" 
  , "  for(int k=0; k<" ++ show (2*extDegree-1) ++ "; k++) {"
  , "    int a = MAX( 0 , k-" ++ show (extDegree-1) ++ " );"
  , "    int b = MIN( k ,   " ++ show (extDegree-1) ++ " );"
  , "    void " ++ base_prefix ++ "set_zero( PROD(k) );" 
  , "    for(int i=a; i<=b; i++) {"
  , "      int j = k-i;"
  , "      " ++ base_prefix ++ "mul( SRC1(i) , SRC2(j) , tmp );" 
  , "      " ++ base_prefix ++ "add_inplace( PROD(k) , tmp );" 
  , "    }"
  , "  }"
  , "  for(int k=" ++ show (2*extDegree-2) ++ "; k>" ++ show (extDegree-1) ++ "; k--) {"
  , "    " ++ prefix ++ "subtract_irred_generic( PROD(k) , PROD(k-EXT_DEGREE) );"
  , "  }"
  , "}"
  ]

c_mulExtInplace :: ExtParams -> Code
c_mulExtInplace ExtParams{..} =  
  [ ""
  , "void " ++ prefix ++ "mul_inplace ( uint64_t *tgt , const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "mul( tgt, src2, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sqr_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "sqr( tgt, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

c_mulExtQuadratic :: ExtParams -> Code
c_mulExtQuadratic ExtParams{..} =  
  [ "// we use Karatsuba trick to have only 3 multiplications"
  , "void " ++ prefix ++ "mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t p[BASE_NWORDS];"
  , "  uint64_t q[BASE_NWORDS];"
  , "  uint64_t r[BASE_NWORDS];"
  , "  uint64_t tmp[BASE_NWORDS];"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(0) , p );         // a0*b0"
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(1) , r );         // a1*b1"
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(1) , q );         // (a0+a1)"
  , "  " ++ base_prefix ++ "add( SRC2(0) , SRC2(1) , tmp );       // (b0+b1)"
  , "  " ++ base_prefix ++ "mul_inplace( q , tmp );               // (a0+a1)*(b0+b1)"
  , "  " ++ base_prefix ++ "sub_inplace( q , p );"
  , "  " ++ base_prefix ++ "sub_inplace( q , r );"
  ] ++
  withIrredCoeffs irredPoly termDeg0 ++
  withIrredCoeffs irredPoly termDeg1 ++ 
  [ "}"
  , ""
  , "// we use Karatsuba trick to have only 3 squarings"
  , "void " ++ prefix ++ "sqr ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t p[BASE_NWORDS];"
  , "  uint64_t q[BASE_NWORDS];"
  , "  uint64_t r[BASE_NWORDS];"
  , "  uint64_t tmp[BASE_NWORDS];"
  , "  " ++ base_prefix ++ "sqr( SRC1(0) , p );              // a0^2"
  , "  " ++ base_prefix ++ "sqr( SRC1(1) , r );              // a1^2"
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(1) , q );    // (a0+a1)"
  , "  " ++ base_prefix ++ "sqr_inplace( q );                // (a0+a1)^2"
  , "  " ++ base_prefix ++ "sub_inplace( q , p );"
  , "  " ++ base_prefix ++ "sub_inplace( q , r );"
  ] ++
  withIrredCoeffs irredPoly termDeg0 ++
  withIrredCoeffs irredPoly termDeg1 ++ 
  [ "}"
  , ""
  ] 
{-
  [ "void " ++ prefix ++ "mul_naive ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t p[BASE_NWORDS];"
  , "  uint64_t q[BASE_NWORDS];"
  , "  uint64_t r[BASE_NWORDS];"
  , "  uint64_t tmp[BASE_NWORDS];"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(0) , p );"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(1) , q );"
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(0) , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( q , tmp );"
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(1) , r );"
  ] ++
  withIrredCoeffs irredPoly termDeg0 ++
  withIrredCoeffs irredPoly termDeg1 ++ 
  [ "}"
  , ""
  , "void " ++ prefix ++ "sqr_naive ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t p[BASE_NWORDS];"
  , "  uint64_t q[BASE_NWORDS];"
  , "  uint64_t r[BASE_NWORDS];"
  , "  uint64_t tmp[BASE_NWORDS];"
  , "  " ++ base_prefix ++ "sqr( SRC1(0) , p );"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC1(1) , q );"
  , "  " ++ base_prefix ++ "add_inplace( q , q );"
  , "  " ++ base_prefix ++ "sqr( SRC1(1) , r );"
  ] ++
  withIrredCoeffs irredPoly termDeg0 ++
  withIrredCoeffs irredPoly termDeg1 ++ 
  [ "}"
  , ""
  ]
-}
  where
    
    termDeg0 [c,d]  
      | isZero     c  = [ "  " ++ base_prefix ++ "copy( p ,     TGT(0) );" ]
      | isOne      c  = [ "  " ++ base_prefix ++ "sub(  p , r , TGT(0) );" ]
      | isMinusOne c  = [ "  " ++ base_prefix ++ "add(  p , r , TGT(0) );" ]
      | otherwise    =  [ "  " ++ base_prefix ++ "mul(  r , IRRED(0) , tmp );"
                        , "  " ++ base_prefix ++ "sub(  p , tmp , TGT(0) );" 
                        ]

    termDeg1 [c,d]
      | isZero     d  = [ "  " ++ base_prefix ++ "copy( q ,     TGT(1) );" ]
      | isOne      d  = [ "  " ++ base_prefix ++ "sub(  q , r , TGT(1) );" ]
      | isMinusOne d  = [ "  " ++ base_prefix ++ "add(  q , r , TGT(1) );" ]
      | otherwise    =  [ "  " ++ base_prefix ++ "mul(  r , IRRED(1) , tmp );"
                        , "  " ++ base_prefix ++ "sub(  q , tmp , TGT(1) );" 
                        ]

--------------------------------------------------------------------------------

c_mulExtCubic :: ExtParams -> Code
c_mulExtCubic ExtParams{..} =  
  [ "// we use Karatsuba trick to have only 6 multiplications"
  , "void " ++ prefix ++ "mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t prod[5*BASE_NWORDS];"
  , "  uint64_t tmp [  BASE_NWORDS];"
  , "  uint64_t tmp2[  BASE_NWORDS];"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(0) , PROD(0) );      // p = a0 * b0"
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(1) , PROD(2) );      // q = a1 * b1"
  , "  " ++ base_prefix ++ "mul( SRC1(2) , SRC2(2) , PROD(4) );      // r = a2 * b2"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(1) , PROD(1) );      // (a0+a1)"
  , "  " ++ base_prefix ++ "add( SRC2(0) , SRC2(1) , tmp );          // (b0+b1)"
  , "  " ++ base_prefix ++ "mul_inplace( PROD(1) , tmp );            // (a0+a1)*(b0+b1)"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(1) , PROD(0) );"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(1) , PROD(2) );        // a0*b1 + a1*b0"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(1) , SRC1(2) , PROD(3) );      // (a1+a2)"
  , "  " ++ base_prefix ++ "add( SRC2(1) , SRC2(2) , tmp );          // (b1+b2)"
  , "  " ++ base_prefix ++ "mul_inplace( PROD(3) , tmp );            // (a1+a2)*(b1+b2)"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(3) , PROD(2) );"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(3) , PROD(4) );        // a0*b1 + a1*b0"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(2) , tmp2 );         // (a0+a2)"
  , "  " ++ base_prefix ++ "add( SRC2(0) , SRC2(2) , tmp  );         // (b0+b2)"
  , "  " ++ base_prefix ++ "mul_inplace( tmp2 , tmp );               // (a0+a2)*(b0+b2)"
  , "  " ++ base_prefix ++ "sub_inplace( tmp2 , PROD(0) );"
  , "  " ++ base_prefix ++ "sub_inplace( tmp2 , PROD(4) );           // a0*b2 + a2*b0"
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , tmp2 );           // a0*b2 + a1*b1 + a2*b0"  
  ] ++
  subtract_ts ++
  [ "  memcpy( tgt, prod, 8*EXT_NWORDS );"
  , "}"
  , ""
  , "// we use Karatsuba trick to have only 6 squarings"
  , "void " ++ prefix ++ "sqr ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t prod[5*BASE_NWORDS];"
  , "  uint64_t tmp [  BASE_NWORDS];"
  , "  " ++ base_prefix ++ "sqr( SRC1(0) , PROD(0) );                // p = a0^2"
  , "  " ++ base_prefix ++ "sqr( SRC1(1) , PROD(2) );                // q = a1^2"
  , "  " ++ base_prefix ++ "sqr( SRC1(2) , PROD(4) );                // r = a2^2"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(1) , PROD(1) );      // (a0+a1)"
  , "  " ++ base_prefix ++ "sqr_inplace( PROD(1) );                  // (a0+a1)^2"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(1) , PROD(0) );"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(1) , PROD(2) );        // a0*a1 + a1*a0"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(1) , SRC1(2) , PROD(3) );      // (a1+a2)"
  , "  " ++ base_prefix ++ "sqr_inplace( PROD(3) );                  // (a1+a2)^2"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(3) , PROD(2) );"
  , "  " ++ base_prefix ++ "sub_inplace( PROD(3) , PROD(4) );        // a1*a2 + a2*a1"
  , ""
  , "  " ++ base_prefix ++ "add( SRC1(0) , SRC1(2) , tmp  );         // (a0+a2)"
  , "  " ++ base_prefix ++ "sqr_inplace( tmp   );                    // (a0+a2)^2"
  , "  " ++ base_prefix ++ "sub_inplace( tmp , PROD(0) );"
  , "  " ++ base_prefix ++ "sub_inplace( tmp , PROD(4) );            // a0*a2 + a2*a0"
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , tmp );            // a0*a2 + a1*a1 + a2*a0"  
  ] ++
  subtract_ts ++
  [ "  memcpy( tgt, prod, 8*EXT_NWORDS );"
  , "}"
  , ""
  ] 
{-
  [ "void " ++ prefix ++ "mul_naive ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t prod[5*BASE_NWORDS];"
  , "  uint64_t tmp [  BASE_NWORDS];"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(0) , PROD(0) );     // p = a0 * b0"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(1) , PROD(1) );"   
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(0) , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(1) , tmp );           // q = a0*b1 + a1*b0"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC2(2) , PROD(2) );"   
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(1) , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , tmp );"
  , "  " ++ base_prefix ++ "mul( SRC1(2) , SRC2(0) , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , tmp );           // r = a0*b2 + a1*b1 + a2*b0"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC2(2) , PROD(3) );"
  , "  " ++ base_prefix ++ "mul( SRC1(2) , SRC2(1) , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(3) , tmp );           // s = a1*b2 + a2*b1"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(2) , SRC2(2) , PROD(4) );     // t = a2*b2"
  ] ++
  subtract_ts ++
  [ "  memcpy( tgt, prod, 8*EXT_NWORDS );"
  , "  // " ++ base_prefix ++ "copy( PROD(0) , TGT(0) );" 
  , "  // " ++ base_prefix ++ "copy( PROD(1) , TGT(1) );" 
  , "  // " ++ base_prefix ++ "copy( PROD(2) , TGT(2) );" 
  , "}"
  , ""
  , "void " ++ prefix ++ "sqr_naive ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t prod[5*BASE_NWORDS];"
  , "  uint64_t tmp [  BASE_NWORDS];"
  , "  " ++ base_prefix ++ "sqr( SRC1(0) , PROD(0) );               // p = a0^2"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC1(1) , PROD(1) );"   
  , "  " ++ base_prefix ++ "add_inplace( PROD(1) , PROD(1) );       // q = 2*a0*a1"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC1(2) , PROD(2) );"   
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , PROD(2) );"         
  , "  " ++ base_prefix ++ "sqr( SRC1(1)   , tmp );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(2) , tmp );           // r = 2*a0*a2 + a1^2"
  , ""
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC1(2) , PROD(3) );"
  , "  " ++ base_prefix ++ "add_inplace( PROD(3) , PROD(3) );       // s = 2*a1*a2"
  , ""
  , "  " ++ base_prefix ++ "sqr( SRC1(2) , PROD(4) );               // t = a2^2"
  ] ++
  subtract_ts ++
  [ "  memcpy( tgt, prod, 8*EXT_NWORDS );"
  , "  // " ++ base_prefix ++ "copy( PROD(0) , TGT(0) );" 
  , "  // " ++ base_prefix ++ "copy( PROD(1) , TGT(1) );" 
  , "  // " ++ base_prefix ++ "copy( PROD(2) , TGT(2) );" 
  , "}"
  , ""
  ]
-}
  where
    subtract_ts =
      [ "  " ++ prefix ++ "subtract_irred_generic( PROD(4) , PROD(1) );"
      , "  " ++ prefix ++ "subtract_irred_generic( PROD(3) , PROD(0) );"
      ]

--------------------------------------------------------------------------------

c_invExt :: ExtParams -> Code
c_invExt extparams@(ExtParams{..}) 
 | extDegree <= 1   = error "fatal error: field extension degree must be > 1"
 | extDegree == 2   = c_invExtQuadratic    extparams
 | extDegree == 3   = c_invExtCubic        extparams
 | otherwise        = c_invExtGeneric      extparams
-- | otherwise        =  error ("inverse is not yet implemented for field extension of degree " ++ show extDegree)

c_divExt :: ExtParams -> Code
c_divExt extparams@(ExtParams{..}) 
 | extDegree <= 1   = error "fatal error: field extension degree must be > 1"
 | otherwise        = c_divExtGeneric      extparams
-- | extDegree == 2   = c_divExtQuadratic    extparams

----------------------------------------

-- | We can solve the equation explicitly.
--
-- > irred = x^2 + p*x + q
-- > (a*x + b) * (c*x + d) = (a*c)*x^2 + (a*d+b*c)*x + (b*d)
-- >                       = (a*d + b*c - a*c*p)*x + (b*d - a*c*q)
--
-- and then we want to solve
--
-- > b*d       - a*c*q == 1
-- > a*d + b*c - a*c*p == 0
--
-- which has the solution:
--
-- > c = - a       / (b^2 - a*b*p + a^2*q)  
-- > d = (b - a*p) / (b^2 - a*b*p + a^2*q)
--
-- Remark: It seems the denominator being zero would mean that our
-- defining polynomial is not irreducible.
--
-- Note: we can optimize for the common case p=0; and also for q=1.
--
c_invExtQuadratic ::  ExtParams -> Code
c_invExtQuadratic ExtParams{..} =  
  [ "void " ++ prefix ++ "inv ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t denom     [BASE_NWORDS];"
  , "  uint64_t b_minus_ap[BASE_NWORDS];"
  , "  uint64_t tmp       [BASE_NWORDS];"
  ] ++
  (if p_is_zero
    then
      [ "  " ++ base_prefix ++ "copy( SRC1(0) , b_minus_ap );                   // b-a*p = b (because p=0)"
      ]
    else
      [ "  " ++ base_prefix ++ "mul( SRC1(1) , IRRED(1) , b_minus_ap );         // a*p"
      , "  " ++ base_prefix ++ "sub_inplace_reverse( b_minus_ap , SRC1(0) );    // b-a*p"
      , "  // " ++ base_prefix ++ "sub_inplace( b_minus_ap , SRC1(0) );    // a*p - b"
      , "  // " ++ base_prefix ++ "neg_inplace( b_minus_ap );              // b - a*p"
      ]
  ) ++
  [ "  " ++ base_prefix ++ "sqr( SRC1(1) , denom );                         // a^2"
  ] ++
  (if q_is_one
    then []
    else [ "  " ++ base_prefix ++ "mul_inplace( denom , IRRED(0) );                // a^2*q" 
         ]
  ) ++
  [ "  " ++ base_prefix ++ "mul( SRC1(0) , b_minus_ap , tmp );              // b*(b-a*p) = b^2-a*b*p"
  , "  " ++ base_prefix ++ "add_inplace( denom , tmp );                     // (b^2 - a*b*p) + a^2*q"
  , "  " ++ base_prefix ++ "inv_inplace( denom );                           // 1/(b^2 - a*b*p - a^2*q)"
  , "  " ++ base_prefix ++ "neg( SRC1(1) , tmp );                           // -a"
  , "  " ++ base_prefix ++ "mul( tmp        , denom , TGT(1) );             // c := -a/denom"
  , "  " ++ base_prefix ++ "mul( b_minus_ap , denom, TGT(0) );              // d := (b-a*p)/denom"
  , "}"
  , ""  
  , "void " ++ prefix ++ "inv_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "inv ( tgt , tgt ); "
  , "}"
  ]  
  where
    p_is_zero = case irredPoly of { AnyIrredPoly (IrredPoly [_,p]) -> isZero p }
    q_is_one  = case irredPoly of { AnyIrredPoly (IrredPoly [q,_]) -> isOne  q }

--------------------------------------------------------------------------------

c_invExtCubic ::  ExtParams -> Code
c_invExtCubic extparams@(ExtParams{..}) = case irredPoly of
  AnyIrredPoly (IrredPoly [p0,p1,p2])
    | isZero p1 && isZero p2  -> c_invExtCubic00 extparams
    | otherwise               -> error "c_invExtCubic: inversion in general cubic extension is not yet implemented"

-- | Here we assume that the defininig polynomial has the form @(x^3 + p0)@. 
--
-- > denom := ( a0^3 - a1^3 p0 + 3 a0 a1 a2 p0 + a2^3 p0^2 )
-- >
-- > b0 ->   ( a0^2 + a1 a2 p0 ) / denom
-- > b1 -> - ( a0 a1 + a2^2 p0 ) / denom
-- > b2 ->   ( a0 a2 - a1^2    ) / denom
--
c_invExtCubic00 ::  ExtParams -> Code
c_invExtCubic00 extparams@(ExtParams{..}) = 
  [ "void " ++ prefix ++ "inv ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  uint64_t denom     [BASE_NWORDS];"
  , "  uint64_t a0a0[BASE_NWORDS];"
  , "  uint64_t a1a1[BASE_NWORDS];"
  , "  uint64_t a2a2[BASE_NWORDS];"
  , "  uint64_t a0a1[BASE_NWORDS];"
  , "  uint64_t a0a2[BASE_NWORDS];"
  , "  uint64_t a1a2[BASE_NWORDS];"
  , "  uint64_t a1a1a1[BASE_NWORDS];"
  , "  uint64_t a1a2p0[BASE_NWORDS];"
  , "  uint64_t a2a2p0[BASE_NWORDS];"
  , "  uint64_t tmp[BASE_NWORDS];"
  , "  " ++ base_prefix ++ "sqr( SRC1(0) , a0a0 );                // a0^2"
  , "  " ++ base_prefix ++ "sqr( SRC1(1) , a1a1 );                // a1^2"
  , "  " ++ base_prefix ++ "sqr( SRC1(2) , a2a2 );                // a2^2"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC1(1) , a0a1 );      // a0*a1"
  , "  " ++ base_prefix ++ "mul( SRC1(0) , SRC1(2) , a0a2 );      // a0*a2"
  , "  " ++ base_prefix ++ "mul( SRC1(1) , SRC1(2) , a1a2 );      // a1*a2"
  , "  " ++ base_prefix ++ "mul( a1a1 , SRC1(1)  , a1a1a1 );      // a1^3"
  , "  " ++ base_prefix ++ "mul( a1a2 , IRRED(0) , a1a2p0 );      // a1*a2*p0"
  , "  " ++ base_prefix ++ "mul( a2a2 , IRRED(0) , a2a2p0 );      // a2^2*p0"
  , "  // --- "
  , "  " ++ base_prefix ++ "add( a1a2p0 , a1a2p0 , denom );       // 2*a1*a2*p0"
  , "  " ++ base_prefix ++ "add_inplace( denom , a1a2p0  );       // 3*a1*a2*p0"
  , "  " ++ base_prefix ++ "add_inplace( denom , a0a0    );       // a0^2 + 3*a1*a2*p0"
  , "  " ++ base_prefix ++ "mul_inplace( denom , SRC1(0) );       // a0^3 + 3*a0*a1*a2*p0"
  , "  " ++ base_prefix ++ "mul( a2a2p0 , SRC1(2) , tmp );        // a2^3*p0"
  , "  " ++ base_prefix ++ "sub_inplace( tmp , a1a1a1 );          // a2^3*p0 - a1^3"
  , "  " ++ base_prefix ++ "mul_inplace( tmp , IRRED(0) );        // a2^3*p0^2 - a1^3*p0"
  , "  " ++ base_prefix ++ "add_inplace( denom, tmp );"
  , "  // --- "
  , "  " ++ base_prefix ++ "inv_inplace( denom );"
  , "  " ++ base_prefix ++ "add( a0a0 , a1a2p0 , TGT(0) );         // a0*a0 + a1*a2*p0"
  , "  " ++ base_prefix ++ "add( a0a1 , a2a2p0 , TGT(1) );         // a0*a1 + a2^2*p0"
  , "  " ++ base_prefix ++ "neg_inplace( TGT(1) );                 // -(a0*a1 + a2^2*p0)"
  , "  " ++ base_prefix ++ "sub( a1a1 , a0a2    , TGT(2) );        // a1^2 - a0*a2"
  , "  " ++ base_prefix ++ "mul_inplace( TGT(0) , denom );"    
  , "  " ++ base_prefix ++ "mul_inplace( TGT(1) , denom );"    
  , "  " ++ base_prefix ++ "mul_inplace( TGT(2) , denom );"    
  , "}"
  , ""  
  , "void " ++ prefix ++ "inv_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "inv ( tgt , tgt ); "
  , "}"
  ]  

--------------------------------------------------------------------------------

c_invExtGeneric :: ExtParams -> Code
c_invExtGeneric ExtParams{..} =  
  [ "// TEMP PLACEHOLDER"
  , "void " ++ prefix ++ "inv ( const uint64_t *src1, uint64_t *tgt ) {"
  , "  " ++ prefix ++ "set_zero ( tgt );"
  , "}"
  , ""  
  , "void " ++ prefix ++ "inv_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "inv ( tgt , tgt );"
  , "}"
  ]  

c_divExtGeneric :: ExtParams -> Code
c_divExtGeneric ExtParams{..} =  
  [ "void " ++ prefix ++ "div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t inv[EXT_NWORDS];"
  , "  " ++ prefix ++ "inv ( src2 , inv );"
  , "  " ++ prefix ++ "mul ( src1 , inv , tgt );"
  , "}"
  , ""  
  , "void " ++ prefix ++ "div_inplace ( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "div ( tgt , src2 , tgt ); "
  , "}"
  ]  

--------------------------------------------------------------------------------
-- frobenius

-- in practice this seems to be very sparse, so a dense representation is suboptimal
frobeniusBasesDense :: forall f. (Field f, SerializeMontgomery f, ExtField' f) => Proxy f -> [[Word64]]
frobeniusBasesDense proxy = map toWordsMontgomery ys where
  m  = primeDeg proxy
  bs = [ packPrimeBase (ei i) | i<-[0..m-1] ] :: [f]
  ys = map frobenius bs

  ei :: Int -> [PrimeBase f]
  ei i = replicate i 0 ++ [1] ++ replicate (m-i-1) 0

c_frobenius_dense :: ExtParams -> Code
c_frobenius_dense ExtParams{..} = 
  [ ""
  , mkConstWordArray (prefix ++ "frobenius_basis") frobBases
  , "" 
  , "void " ++ prefix ++ "frobenius ( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint64_t acc[EXT_NWORDS];"
  , "  uint64_t tmp[EXT_NWORDS];"
  , "  " ++ prefix ++ "set_zero(acc);"
  , "  for(int i=0; i<PRIME_DEGREE; i++) {"
  , "    " ++ prefix ++ "scale_by_prime_field( src + i*PRIME_NWORDS , " ++ prefix ++ "frobenius_basis + i*EXT_NWORDS , tmp );"
  , "    " ++ prefix ++ "add_inplace( acc, tmp );"
  , "  }"
  , "  " ++ prefix ++ "copy( acc, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "frobenius_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "frobenius( tgt, tgt );"
  , "}"
  ] 
  where 
    frobBases = case pureTypeProxy of
      AnyExtProxy proxy -> frobeniusBasesDense proxy

----------------------------------------

frobeniusBasesSparse :: forall f. (Field f, SerializeMontgomery f, ExtField' f) => Proxy f -> ( [(Int,Int)] , [[Word64]] )
frobeniusBasesSparse proxy = (indices, map toWordsMontgomery values) where
  m  = primeDeg proxy
  bs = [ packPrimeBase (ei i) | i<-[0..m-1] ] :: [f]
  ys = map frobenius bs :: [f]

  ei :: Int -> [PrimeBase f]
  ei i = replicate i 0 ++ [1] ++ replicate (m-i-1) 0

  list   = zip [0..] (map (zip [0..]) $ map unpackPrimeBase ys) :: [(Int,[(Int,PrimeBase f)])]
  list'  = concatMap f list :: [((Int,Int),PrimeBase f)]
  f (i,jcs) = [ ((i,j),c) | (j,c) <- jcs ]
  list'' = filter (not . isZero . snd) list'
  (indices,values) = unzip list''

c_frobenius_sparse :: ExtParams -> Code
c_frobenius_sparse ExtParams{..} = 
  [ ""
  , mkConstWordArray (prefix ++ "frobenius_sparse_indices") [indices]
  , "" 
  , mkConstWordArray (prefix ++ "frobenius_sparse_entries") ws
  , "" 
  , "void " ++ prefix ++ "frobenius ( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint64_t acc[EXT_NWORDS];"
  , "  uint64_t tmp[PRIME_NWORDS];"
  , "  " ++ prefix ++ "set_zero(acc);"
  , "  for(int k=0; k<" ++ show nentries ++ "; k++) {"
  , "    uint64_t ij = " ++ prefix ++ "frobenius_sparse_indices[k];"
  , "    uint64_t i  = (ij >> 16);"
  , "    uint64_t j  = (ij &  0xffff);"
  , "    " ++ prime_prefix ++ "mul( src + i*PRIME_NWORDS , " ++ prefix ++ "frobenius_sparse_entries + k*PRIME_NWORDS , tmp );"
  , "    " ++ prime_prefix ++ "add_inplace( acc + j*PRIME_NWORDS , tmp );"
  , "  }"
  , "  " ++ prefix ++ "copy( acc, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "frobenius_inplace ( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "frobenius( tgt, tgt );"
  , "}"
  ] 
  where 
    nentries = length ijs
    indices  = [ fromIntegral (shiftL i 16 + j) | (i,j) <- ijs ] :: [Word64]
    (ijs,ws)   = case pureTypeProxy of
      AnyExtProxy proxy -> frobeniusBasesSparse proxy

--------------------------------------------------------------------------------

hsBegin :: ExtParams -> Code
hsBegin extparams@(ExtParams{..}) =
  [ "-- | Algebraic field extension `" ++ extFieldName ++ "`"
  , "--"
  , "-- * NOTE 1: This module is intented to be imported qualified"
  , "--"
  , "-- * NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}"
  , "module " ++ hsModule hs_path
  , "  ( " ++ typeName ++ "(..)"
  , "    -- * Conversion"
  , "  , pack" ++ postfix ++ " , unpack" ++ postfix 
  , "  , packList" ++ postfix ++ " , unpackList" ++ postfix 
  , "  , packListPrime" ++ postfix ++ " , unpackListPrime" ++ postfix 
  , "    -- * Field elements"
  , "  , zero , one , two     -- , primGen"
  , "    -- * Predicates"
  , "  , isValid , isZero , isOne , isEqual"
  , "    -- * Field operations"
  , "  , neg , add , sub"
  , "  , sqr , mul"
  , "  , inv , div , divBy2 , batchInv"
  , "    -- * Exponentiation"
  , "  , pow , pow_"
  , "    -- * Relation to the base and prime fields"
  , "  , embedBase" ++ postfix ++ " , embedPrime" ++ postfix 
  , "  , scaleBase" ++ postfix ++ " , scalePrime" ++ postfix 
  , "    -- * Frobenius automorphism"
  , "  , frob"
  , "    -- * Random"
  , "  , rnd"
  , "    -- * Export to C"
  , "  , exportToCDef , exportListToCDef"
  , "  )"  
  , "  where"  
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "import Prelude  hiding (div)"
  , "import GHC.Real hiding (div)"
  , ""
  , "import Data.Bits"
  , "import Data.Word"
  , "import Data.Proxy"
  , ""
  , "import Foreign.C"
  , "import Foreign.Ptr"
  , "import Foreign.Marshal"
  , "import Foreign.ForeignPtr"
  , ""
  , "import System.Random"
  , "import System.IO.Unsafe"
  , ""
  , "import ZK.Algebra.BigInt." ++ expoBigintType ++ " (" ++ expoBigintType ++ "(..) )"
  , ""
  , "import " ++ hsModule  hs_path_base  ++ "( " ++ typeNameBase ++ "(..) )"
  , "import " ++ hsModule  hs_path_prime ++ "( " ++ typeNamePrime ++ "(..) )"
  , "import qualified " ++ hsModule  hs_path_base ++ " as Base"
  , "import qualified " ++ hsModule  hs_path_prime ++ " as Prime"
  , ""
  , "import           ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as C"
  , "import ZK.Algebra.Helpers"
  , ""
  , "--------------------------------------------------------------------------------  "
  , ""
--  , "foreign import ccall \"" ++ prefix ++ "irred_coeffs\" c_" ++ prefix ++ "irred_coeffs :: Ptr Word64"
--  , ""
--  , "{-# NOINLINE theDefiningPoly #-}
--  , "-- | The truncated defining polynomial of the field extension"
--  , "theDefiningPoly :: " ++ typeName
--  , "theDefiningPoly = unsafePerformIO $ makeFlat c_" ++ prefix ++ "irred_coeffs"
--  , ""
--  , "--------------------------------------------------------------------------------  "
  , ""
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "zero, one, two :: " ++ typeName
  , "zero = embedPrime" ++ postfix ++ " 0"
  , "one  = embedPrime" ++ postfix ++ " 1"
  , "two  = embedPrime" ++ postfix ++ " 2"
  , ""
  , "primGen :: " ++ typeName
  , "primGen = error \"primGen/" ++ extFieldName ++ ": not implemented\""
  , ""
  , "instance Eq " ++ typeName ++ " where"
  , "  (==) = isEqual"
  , ""
  , "instance Num " ++ typeName ++ " where"
  , "  fromInteger = embedPrime . fromInteger"
  , "  negate = neg"
  , "  (+) = add"
  , "  (-) = sub"
  , "  (*) = mul" 
  , "  abs    = id"
  , "  signum = \\_ -> one"
  , ""
  , "instance Fractional " ++ typeName ++ " where"
  , "  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)"
  , "  recip = inv"
  , "  (/)   = div"
  , ""
  , "instance Show " ++ typeName ++ " where"
  , "  show = show . unpack" ++ postfix ++ "    -- temp. TODO: proper show"
  , ""
  , "instance L.Flat " ++ typeName ++ " where"
  , "  sizeInBytes  _pxy = " ++ show (8 * extNWords extparams)
  , "  sizeInQWords _pxy = " ++ show (    extNWords extparams)
  , "  withFlat (Mk" ++ typeName ++ " fptr) = withForeignPtr fptr"
  , "  makeFlat = L.makeFlatGeneric Mk" ++ typeName ++ " " ++ show (extNWords extparams)
  , ""
  , "rnd :: IO " ++ typeName
  , "rnd = do"
  ] ++
  [ "  x" ++ show i ++ " <- Base.rnd"
  | i <- [1..extDegree]
  ] ++ 
  [ "  return $ pack" ++ postfix ++ " " ++ tuple' [  "x" ++ show i | i<-[1..extDegree] ]
  , ""
  , "instance C.Rnd " ++ typeName ++ " where"
  , "  rndIO = rnd"
  , ""
  , "instance C.Ring " ++ typeName ++ " where"
  , "  ringNamePxy _ = \"" ++ extFieldName  ++ "\""
  , "  ringSizePxy _ = C.ringSizePxy (Proxy @" ++ typeNameBase ++ ") ^ " ++ show extDegree
  , "  isZero = " ++ hsModule hs_path ++ ".isZero"
  , "  isOne  = " ++ hsModule hs_path ++ ".isOne"
  , "  zero   = " ++ hsModule hs_path ++ ".zero"
  , "  one    = " ++ hsModule hs_path ++ ".one"
  , "  square = " ++ hsModule hs_path ++ ".sqr"
  , "  power  = " ++ hsModule hs_path ++ ".pow"
  , ""
  , "instance C.Field " ++ typeName ++ " where"
  , "  charPxy    _ = C.charPxy (Proxy @" ++ typeNameBase ++ ")"
  , "  dimPxy     _ = C.dimPxy  (Proxy @" ++ typeNameBase ++ ") * " ++ show extDegree  
  , "  primGenPxy _ = primGen"
  , "  batchInverse = batchInv"
  , "  frobenius    = frob"
  , "  halve        = divBy2"
  , ""
  , "instance C.ExtField " ++ typeName ++ " where"
  , "  type ExtBase " ++ typeName ++ " = " ++ typeNameBase
  , "  extDeg _ = " ++ show extDegree
  , "  definingPolyCoeffs = error \"definingPolyCoeffs: not yet implemented\""
  , "  embedExtBase     = embedBase"
  , "  projectToExtBase = error \"projectToExtBase: not yet implemented\""
  , "  scaleExtBase     = scaleBase"
  , "  extPack          = packList"
  , "  extUnpack        = unpackList"
  , ""
  , "instance C.ExtField' " ++ typeName ++ " where"
  , "  type PrimeBase " ++ typeName ++ " = " ++ typeNamePrime
  , "  embedPrimeField     = embedPrime"
  , "  projectToPrimeField = error \"projectToPrimeField: not yet implemented\""
  , "  scalePrimeField     = scalePrime"
  , "  primePack           = packListPrime"
  , "  primeUnpack         = unpackListPrime"
  , ""
  ] ++
  (case extDegree of
    2 -> [ "instance C.QuadraticExt " ++ typeName ++ " where"
         , "  quadraticUnpack = unpack" ++ postfix
         , "  quadraticPack   = pack"   ++ postfix
         ]
    3 -> [ "instance C.CubicExt " ++ typeName ++ " where"
         , "  cubicUnpack = unpack" ++ postfix
         , "  cubicPack   = pack"   ++ postfix
         ]
    _ -> []
  ) ++
  [ ""
  , "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix       ++ "set_const\" c_" ++ prefix      ++ "set_const :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "foreign import ccall unsafe \"" ++ base_prefix  ++ "copy\"      c_" ++ base_prefix  ++ "copy      :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , (if base_prefix == prime_prefix then "" else 
      "foreign import ccall unsafe \"" ++ prime_prefix ++ "copy\"      c_" ++ prime_prefix ++ "copy      :: Ptr Word64 -> Ptr Word64 -> IO ()")
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "from_base_field\"  c_" ++ prefix ++ "from_base_field  :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ prefix ++ "from_prime_field\" c_" ++ prefix ++ "from_prime_field :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "scale_by_base_field\"  c_" ++ prefix ++ "scale_by_base_field  :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ prefix ++ "scale_by_prime_field\" c_" ++ prefix ++ "scale_by_prime_field :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "----------------------------------------"
  , ""
  , "mallocForeignTuple :: IO " ++ tuple "ForeignPtr Word64"
  , "mallocForeignTuple = do"
  ] ++ 
  [ "  fptr" ++ show i ++ " <- mallocForeignPtrArray " ++ show (baseNWords)
  | i <- [1..extDegree]
  ] ++ 
  [ "  return " ++ tuple' [ "fptr" ++ show i | i <- [1..extDegree] ]
  , ""
  , "withForeignTuple :: " ++ tuple "ForeignPtr Word64" ++ " -> (" ++ tuple "Ptr Word64" ++ " -> IO a) -> IO a"
  , "withForeignTuple " ++ tuple' [ "fptr" ++ show i | i <- [1..extDegree] ] ++ " action = do"
  ] ++
  [ "  withForeignPtr fptr" ++ show i ++ " $ \\ptr" ++ show i ++ " -> do"
  | i <- [1..extDegree]
  ] ++ 
  [ "  action " ++ tuple' [ "ptr" ++ show i | i <- [1..extDegree] ] 
  , ""
  , "----------------"
  , ""
  , "mallocForeignList :: IO " ++ list "ForeignPtr Word64"
  , "mallocForeignList = do"
  ] ++ 
  [ "  fptr" ++ show i ++ " <- mallocForeignPtrArray " ++ show (baseNWords)
  | i <- [1..extDegree]
  ] ++ 
  [ "  return " ++ list' [ "fptr" ++ show i | i <- [1..extDegree] ]
  , ""
  , "withForeignList :: " ++ list "ForeignPtr Word64" ++ " -> (" ++ list "Ptr Word64" ++ " -> IO a) -> IO a"
  , "withForeignList " ++ list' [ "fptr" ++ show i | i <- [1..extDegree] ] ++ " action = do"
  ] ++
  [ "  withForeignPtr fptr" ++ show i ++ " $ \\ptr" ++ show i ++ " -> do"
  | i <- [1..extDegree]
  ] ++ 
  [ "  action " ++ list' [ "ptr" ++ show i | i <- [1..extDegree] ] 
  , ""
  , "----------------"
  , ""
  , "mallocForeignListPrime :: IO " ++ list "ForeignPtr Word64"
  , "mallocForeignListPrime = do"
  ] ++ 
  [ "  fptr" ++ show i ++ " <- mallocForeignPtrArray " ++ show (primeNWords)
  | i <- [1..primeDegree]
  ] ++ 
  [ "  return " ++ list' [ "fptr" ++ show i | i <- [1..primeDegree] ]
  , ""
  , "withForeignListPrime :: " ++ list "ForeignPtr Word64" ++ " -> (" ++ list "Ptr Word64" ++ " -> IO a) -> IO a"
  , "withForeignListPrime " ++ list' [ "fptr" ++ show i | i <- [1..primeDegree] ] ++ " action = do"
  ] ++
  [ "  withForeignPtr fptr" ++ show i ++ " $ \\ptr" ++ show i ++ " -> do"
  | i <- [1..primeDegree]
  ] ++ 
  [ "  action " ++ list' [ "ptr" ++ show i | i <- [1..primeDegree] ] 
  , ""
  , "----------------------------------------"
  , ""
  , "{-# NOINLINE embedBase" ++ postfix ++ "#-}"
  , "embedBase" ++ postfix ++ " :: " ++ typeNameBase ++ " -> " ++ typeName 
  , "embedBase" ++ postfix ++ " (Mk" ++ typeNameBase ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "from_base_field ptr1 ptr2"
  , "  return (Mk" ++ typeName ++ " fptr2)"
  , ""
  , "{-# NOINLINE embedPrime" ++ postfix ++ "#-}"
  , "embedPrime" ++ postfix ++ " :: " ++ typeNamePrime ++ " -> " ++ typeName 
  , "embedPrime" ++ postfix ++ " (Mk" ++ typeNamePrime ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "from_prime_field ptr1 ptr2"
  , "  return (Mk" ++ typeName ++ " fptr2)"
  , ""
  , "----------------------------------------"
  , ""
  , "{-# NOINLINE scaleBase" ++ postfix ++ "#-}"
  , "scaleBase" ++ postfix ++ " :: " ++ typeNameBase ++ " -> " ++ typeName  ++ " -> " ++ typeName 
  , "scaleBase" ++ postfix ++ " (Mk" ++ typeNameBase ++ " fptr1) (Mk" ++ typeName ++ " fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "scale_by_base_field ptr1 ptr2 ptr3"
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  , "{-# NOINLINE scalePrime" ++ postfix ++ "#-}"
  , "scalePrime" ++ postfix ++ " :: " ++ typeNamePrime ++ " -> " ++ typeName  ++ " -> " ++ typeName 
  , "scalePrime" ++ postfix ++ " (Mk" ++ typeNamePrime ++ " fptr1) (Mk" ++ typeName ++ " fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "scale_by_prime_field ptr1 ptr2 ptr3"
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  , "----------------------------------------"
  , ""
  , "{-# NOINLINE unpack" ++ postfix ++ " #-}"
  , "unpack" ++ postfix ++ " :: " ++ typeName ++ " -> " ++ tuple typeNameBase
  , "unpack" ++ postfix ++ " (Mk" ++ typeName ++ " fsrc) = unsafePerformIO $ do"
  , "  fptrs@" ++ tuple' [ "ftgt" ++ show i | i <- [1..extDegree] ] ++ " <- mallocForeignTuple"
  , "  withForeignPtr fsrc $ \\src -> do"
  , "    withForeignTuple fptrs $ \\" ++ tuple' [ "tgt" ++ show i | i <- [1..extDegree] ] ++ " -> do"
  ] ++ 
  [ "      c_" ++ base_prefix ++ "copy (plusPtr src " ++ show (8*(i-1)*baseNWords) ++ ") tgt" ++ show i
  | i <- [1..extDegree]
  ] ++ 
  [ "  return " ++ tuple' [ "Mk" ++ typeNameBase ++ " ftgt" ++ show i | i <- [1..extDegree] ] 
  , ""
  , "{-# NOINLINE pack" ++ postfix ++ " #-}"
  , "pack" ++ postfix ++ " :: " ++ tuple typeNameBase ++ " -> " ++ typeName
  , "pack" ++ postfix ++ " " ++ tuple' [ "Mk" ++ typeNameBase ++ " fsrc" ++ show i | i <- [1..extDegree] ] ++ " = unsafePerformIO $ do"
  , "  ftgt <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignTuple " ++ tuple' [ "fsrc" ++ show i | i <- [1..extDegree] ] ++ " $ \\" ++ tuple' [ "src" ++ show i | i <- [1..extDegree] ] ++ " -> do"
  , "    withForeignPtr ftgt $ \\tgt -> do"
  ] ++ 
  [ "      c_" ++ base_prefix ++ "copy src" ++ show i ++ " (plusPtr tgt " ++ show (8*(i-1)*baseNWords) ++ ")"
  | i <- [1..extDegree]
  ] ++ 
  [ "  return (Mk" ++ typeName ++ " ftgt)"
  , ""
  , "----------------------------------------"
  , ""
  , "{-# NOINLINE unpackList" ++ postfix ++ " #-}"
  , "unpackList" ++ postfix ++ " :: " ++ typeName ++ " -> " ++ list typeNameBase
  , "unpackList" ++ postfix ++ " (Mk" ++ typeName ++ " fsrc) = unsafePerformIO $ do"
  , "  fptrs@" ++ list' [ "ftgt" ++ show i | i <- [1..extDegree] ] ++ " <- mallocForeignList"
  , "  withForeignPtr fsrc $ \\src -> do"
  , "    withForeignList fptrs $ \\" ++ list' [ "tgt" ++ show i | i <- [1..extDegree] ] ++ " -> do"
  ] ++ 
  [ "      c_" ++ base_prefix ++ "copy (plusPtr src " ++ show (8*(i-1)*baseNWords) ++ ") tgt" ++ show i
  | i <- [1..extDegree]
  ] ++ 
  [ "  return " ++ list' [ "Mk" ++ typeNameBase ++ " ftgt" ++ show i | i <- [1..extDegree] ] 
  , ""
  , "{-# NOINLINE packList" ++ postfix ++ " #-}"
  , "packList" ++ postfix ++ " :: " ++ list typeNameBase ++ " -> " ++ typeName
  , "packList" ++ postfix ++ " " ++ list' [ "Mk" ++ typeNameBase ++ " fsrc" ++ show i | i <- [1..extDegree] ] ++ " = unsafePerformIO $ do"
  , "  ftgt <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignList " ++ list' [ "fsrc" ++ show i | i <- [1..extDegree] ] ++ " $ \\" ++ list' [ "src" ++ show i | i <- [1..extDegree] ] ++ " -> do"
  , "    withForeignPtr ftgt $ \\tgt -> do"
  ] ++ 
  [ "      c_" ++ base_prefix ++ "copy src" ++ show i ++ " (plusPtr tgt " ++ show (8*(i-1)*baseNWords) ++ ")"
  | i <- [1..extDegree]
  ] ++ 
  [ "  return (Mk" ++ typeName ++ " ftgt)"
  , "packList _ = error \"expecting a list of " ++ show extDegree ++ " " ++ typeNameBase ++ " elements\""
  , ""
  , "----------------------------------------"
  , ""
  , "{-# NOINLINE unpackListPrime" ++ postfix ++ " #-}"
  , "unpackListPrime" ++ postfix ++ " :: " ++ typeName ++ " -> " ++ list typeNamePrime
  , "unpackListPrime" ++ postfix ++ " (Mk" ++ typeName ++ " fsrc) = unsafePerformIO $ do"
  , "  fptrs@" ++ list' [ "ftgt" ++ show i | i <- [1..primeDegree] ] ++ " <- mallocForeignListPrime"
  , "  withForeignPtr fsrc $ \\src -> do"
  , "    withForeignListPrime fptrs $ \\" ++ list' [ "tgt" ++ show i | i <- [1..primeDegree] ] ++ " -> do"
  ] ++ 
  [ "      c_" ++ prime_prefix ++ "copy (plusPtr src " ++ show (8*(i-1)*primeNWords) ++ ") tgt" ++ show i
  | i <- [1..primeDegree]
  ] ++ 
  [ "  return " ++ list' [ "Mk" ++ typeNamePrime ++ " ftgt" ++ show i | i <- [1..primeDegree] ] 
  , ""
  , "{-# NOINLINE packListPrime" ++ postfix ++ " #-}"
  , "packListPrime" ++ postfix ++ " :: " ++ list typeNamePrime ++ " -> " ++ typeName
  , "packListPrime" ++ postfix ++ " " ++ list' [ "Mk" ++ typeNamePrime ++ " fsrc" ++ show i | i <- [1..primeDegree] ] ++ " = unsafePerformIO $ do"
  , "  ftgt <- mallocForeignPtrArray " ++ show (extNWords extparams)
  , "  withForeignListPrime " ++ list' [ "fsrc" ++ show i | i <- [1..primeDegree] ] ++ " $ \\" ++ list' [ "src" ++ show i | i <- [1..primeDegree] ] ++ " -> do"
  , "    withForeignPtr ftgt $ \\tgt -> do"
  ] ++ 
  [ "      c_" ++ prime_prefix ++ "copy src" ++ show i ++ " (plusPtr tgt " ++ show (8*(i-1)*primeNWords) ++ ")"
  | i <- [1..primeDegree]
  ] ++ 
  [ "  return (Mk" ++ typeName ++ " ftgt)"
  , "packListPrime _ = error \"expecting a list of " ++ show primeDegree ++ " " ++ typeNamePrime ++ " elements\""
  , ""
  , "----------------------------------------"
  ] ++
  exportFieldToC     (toCommonParams extparams) ++
  ffi_exponentiation (toCommonParams extparams) ++
  ffi_batch_inverse  (toCommonParams extparams) 

  where 
    postfix = ""  

    tuple :: String -> String
    tuple t = tuple' (replicate extDegree t)

    tuple' :: [String] -> String
    tuple' ts = "(" ++ intercalate ", " ts ++ ")"

    list :: String -> String
    list t = "[" ++ t ++ "]"

    list' :: [String] -> String
    list' ts = "[" ++ intercalate ", " ts ++ "]"

--------------------------------------------------------------------------------

hsFFI :: ExtParams -> Code
hsFFI extparams@(ExtParams{..}) = catCode $ 
  [ mkffi "isZero"      $ cfun  "is_zero"         (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isOne"       $ cfun  "is_one"          (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isEqual"     $ cfun  "is_equal"        (CTyp [CArgInPtr , CArgInPtr              ] CRetBool)
  , mkffi "isValid"     $ cfun  "is_valid"        (CTyp [CArgInPtr                          ] CRetBool)
    --
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sqr"         $ cfun "sqr"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "mul"         $ cfun "mul"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "inv"         $ cfun "inv"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "div"         $ cfun "div"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
    --
  , mkffi "divBy2"      $ cfun "div_by_2"         (CTyp [CArgInPtr ,             CArgOutPtr ] CRetVoid)
    --
  , mkffi "pow_"        $ cfun "pow_uint64"       (CTyp [CArgInPtr , CArg64    , CArgOutPtr ] CRetVoid)
    --
  , mkffi "frob"        $ cfun "frobenius"        (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  ]
  where
    cfun  cname = CFun (prefix ++ cname)
    mkffi = ffiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeName
      , hsTyCon  = "Mk" ++ typeName
      , hsNLimbs = extNWords extparams
      }

--------------------------------------------------------------------------------

c_code :: ExtParams -> Code
c_code extparams = concat $ map ("":)
  [ c_begin         extparams
    --
  , c_basefield        extparams
  , c_zeroOneEtcExt    extparams
  , c_addSubExt        extparams
  , c_mulExt           extparams
  , c_invExt           extparams
  , c_divExt           extparams
  , c_frobenius_sparse extparams
    --
  , exponentiation (toCommonParams extparams)
  , batchInverse   (toCommonParams extparams)
  ]

hs_code :: ExtParams -> Code
hs_code extparams@(ExtParams{..}) = concat $ map ("":)
  [ hsBegin      extparams
  , hsFFI        extparams
  ]

--------------------------------------------------------------------------------

extfield_c_codegen :: FilePath -> ExtParams -> IO ()
extfield_c_codegen tgtdir extparams@(ExtParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header extparams

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code extparams

extfield_hs_codegen :: FilePath -> ExtParams -> IO ()
extfield_hs_codegen tgtdir extparams@(ExtParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code extparams

--------------------------------------------------------------------------------

