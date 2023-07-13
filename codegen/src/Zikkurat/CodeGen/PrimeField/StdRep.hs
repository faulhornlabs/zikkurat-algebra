
-- | Prime fields in standard representation

{-# LANGUAGE BangPatterns, NumericUnderscores, RecordWildCards #-}
module Zikkurat.CodeGen.PrimeField.StdRep where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc
import Zikkurat.CodeGen.FFI
import Zikkurat.Primes -- ( integerLog2 )

--------------------------------------------------------------------------------

data Params = Params 
  { prefix      :: String       -- ^ prefix for C names
  , nlimbs      :: Int          -- ^ number of 64-bit limbs
  , thePrime    :: Integer      -- ^ the prime
  , bigint_     :: String       -- ^ the corresponding bigint prefix, like "bigint256_"
  , c_path      :: Path         -- ^ path of the C module (without extension)
  , hs_path     :: Path         -- ^ path of the Haskell module (without extension) 
  , typeName    :: String       -- ^ the name of the haskell type
  , bigintType  :: String       -- ^ the name of the haskell type of the corresponding BigInt
  , fieldName   :: String       -- ^ name of the field
  , primGen     :: Integer      -- ^ the primitive generator
  }
  deriving Show

--------------------------------------------------------------------------------

c_header :: Params -> Code
c_header (Params{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_valid( const uint64_t *src );"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_zero   ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_one    ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_equal  ( const uint64_t *src1, const uint64_t *src2 );"
  , "extern void    " ++ prefix ++ "set_zero  (       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_one   (       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "copy      ( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "sqr( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "inv( const uint64_t *src1, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "neg_inplace( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sqr_inplace( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "inv_inplace( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );"
  , ""
  , "extern void " ++ prefix ++ "div_by_2           ( const uint64_t *src , uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_by_2_inplace   ( uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "reduce_modp     ( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );"
  , ""
  ]

hsBegin :: Params -> Code
hsBegin (Params{..}) =
  [ "-- | Prime field (standard representation) with"
  , "--"
  , "-- > p = " ++ show thePrime
  , "--"
  , "-- * NOTE 1: This module is intented to be imported qualified"
  , "--"
  , "-- * NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}"
  , "module " ++ hsModule hs_path
  , "  ( " ++ typeName ++ "(..)"
  , "  , prime"
  , "    -- * Conversion"
  , "  , to" ++ postfix 
  , "  , from" ++ postfix 
  , "    -- * Field elements"
  , "  , small , zero , one , two , primGen"
  , "    -- * Predicates"
  , "  , isValid , isZero , isOne , isEqual"
  , "    -- * Field operations"
  , "  , neg , add , sub"
  , "  , sqr , mul"
  , "  , inv , div , div_by_2"
  , "    -- * Exponentiation"
  , "  , pow , pow_"
  , "    -- * Random"
  , "  , rnd"
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
  , ""
  , "import Foreign.C"
  , "import Foreign.Ptr"
  , "import Foreign.Marshal"
  , "import Foreign.ForeignPtr"
  , ""
  , "import System.Random"
  , "import System.IO.Unsafe"
  , ""
  , "import ZK.Algebra.BigInt." ++ bigintType ++ "( " ++ bigintType ++ "(..) )"
  , "import qualified ZK.Algebra.BigInt." ++ bigintType ++ " as B"
  , ""
  , "import qualified ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as C"
  , ""
  , "--------------------------------------------------------------------------------  "
  , ""
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "prime :: Integer"
  , "prime = " ++ show thePrime
  , ""
  , "to" ++ postfix ++ " :: Integer -> " ++ typeName
  , "to" ++ postfix ++ " x = unsafeTo" ++ postfix ++ " (mod x prime)"
  , ""
  , "from" ++ postfix ++ " :: " ++ typeName ++ " -> Integer"
  , "from" ++ postfix ++ " = unsafeFrom" ++ postfix 
  , ""
  , "zero, one, two :: " ++ typeName
  , "zero = small 0"
  , "one  = small 1"
  , "two  = small 2"
  , ""
  , "primGen :: " ++ typeName
  , "primGen = small " ++ show primGen
  , ""
  , "instance Eq " ++ typeName ++ " where"
  , "  (==) = isEqual"
  , ""
  , "instance Num " ++ typeName ++ " where"
  , "  fromInteger = to" ++ postfix
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
  , "  show = show . from" ++ postfix
  , ""
  , "instance L.Flat " ++ typeName ++ " where"
  , "  sizeInBytes  _pxy = " ++ show (8*nlimbs)
  , "  sizeInQWords _pxy = " ++ show (  nlimbs)
  , "  withFlat (Mk" ++ typeName ++ " fptr) = withForeignPtr fptr"
  , "  makeFlat = L.makeFlatGeneric Mk" ++ typeName ++ " " ++ show nlimbs
  , ""
  , "rnd :: IO " ++ typeName
  , "rnd = do"
  , "  x <- randomRIO (0,prime-1)"
  , "  return (unsafeTo" ++ postfix ++ " x)"
  , ""
  , "instance C.Rnd " ++ typeName ++ " where"
  , "  rndIO = rnd"
  , ""
  , "instance C.Ring " ++ typeName ++ " where"
  , "  ringNamePxy _ = \"" ++ fieldName ++ " (standard repr.)\""
  , "  ringSizePxy _ = prime"
  , "  isZero = " ++ hsModule hs_path ++ ".isZero"
  , "  isOne  = " ++ hsModule hs_path ++ ".isOne"
  , "  zero   = " ++ hsModule hs_path ++ ".zero"
  , "  one    = " ++ hsModule hs_path ++ ".one"
  , "  square = " ++ hsModule hs_path ++ ".sqr"
  , "  power x e = pow x (B.to (mod e (prime-1)))"
  , ""
  , "instance C.Field " ++ typeName ++ " where"
  , "  charPxy    _ = prime"
  , "  dimPxy     _ = 1"  
  , "  primGenPxy _ = primGen"
  , ""
  , "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "pow_gen\" c_" ++ prefix ++ "pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()"
  , ""
  , "{-# NOINLINE pow #-}"
  , "pow :: " ++ typeName ++ " -> " ++ bigintType ++ " -> " ++ typeName 
  , "pow (Mk" ++ typeName ++ " fptr1) (Mk" ++ bigintType ++ " fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show nlimbs
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "pow_gen ptr1 ptr2 ptr3 " ++ show (nlimbs)
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  , "----------------------------------------"
  ]
  where 
    postfix = ""  

------------------------------

hsConvert :: Params -> Code
hsConvert (Params{..}) = ffiMarshal "" typeName nlimbs 

hsFFI :: Params -> Code
hsFFI (Params{..}) = catCode $ 
  [ mkffi "isValid"     $ cfun  "is_valid"        (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isZero"      $ cfun_ "is_zero"         (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isOne"       $ cfun_ "is_one"          (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isEqual"     $ cfun_ "is_equal"        (CTyp [CArgInPtr , CArgInPtr              ] CRetBool)
    --
  , mkffi "small"       $ cfun_ "set_small"       (CTyp [CArgOutPtr, CArg64                 ] CRetVoid)
    -- 
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sqr"         $ cfun "sqr"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "mul"         $ cfun "mul"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "inv"         $ cfun "inv"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "div"         $ cfun "div"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
    --
  , mkffi "div_by_2"    $ cfun "div_by_2"         (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "pow_"        $ cfun "pow_uint64"       (CTyp [CArgInPtr , CArg64    , CArgOutPtr ] CRetVoid)
  ]
  where
    cfun_ cname = CFun (bigint_ ++ cname)
    cfun  cname = CFun (prefix  ++ cname)
    mkffi = ffiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeName
      , hsTyCon  = "Mk" ++ typeName
      , hsNLimbs = nlimbs
      }

--------------------------------------------------------------------------------

c_begin :: Params -> Code
c_begin (Params{..}) =
  [ "// finite field arithmetic (standard representation) in the prime field with "
  , "//"
  , "//   p = " ++ show thePrime
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <string.h>"
  , "#include <stdint.h>"
  , "#include <x86intrin.h>"
  , "#include \"" ++ pathBaseName c_path ++ ".h\""
  , "#include \"bigint" ++ show (64*nlimbs) ++ ".h\""
  , ""
  , "#define NLIMBS " ++ show nlimbs
  , ""
  , mkConst nlimbs (prefix ++ "prime") thePrime
  , ""
  , "//------------------------------------------------------------------------------"
  ] 

--------------------------------------------------------------------------------

stdIsOne :: Params -> Code
stdIsOne Params{..} = 
  [ "uint8_t " ++ prefix ++ "is_zero( const uint64_t *src ) {"
  , "  return " ++ bigint_ ++ "is_zero( src );"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_one( const uint64_t *src ) {"
  , "  return " ++ bigint_ ++ "is_one( src );"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_equal( const uint64_t *src1, const uint64_t *src2 ) {"
  , "  return " ++ bigint_ ++ "is_equal( src1 , src2 );"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_zero( uint64_t *tgt ) {"
  , "  " ++ bigint_ ++ "set_zero( tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_one( uint64_t *tgt) {"
  , "  " ++ bigint_ ++ "set_one( tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src, uint64_t *tgt ) {"
  , "  " ++ bigint_ ++ "copy( src , tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------
-- * addition / subtraction

addPrime :: Params -> Code
addPrime Params{..} = 
  [ "// adds the prime p to a bigint"
  , "uint8_t " ++ prefix ++ "" ++ bigint_ ++ "add_prime( const uint64_t *src, uint64_t *tgt ) {"
  , "  return " ++ bigint_ ++ "add( src, " ++ prefix ++ "prime, tgt );"
  , "}"
  , ""
  , "// adds the prime p to a bigint, inplace"
  , "uint8_t " ++ prefix ++ "" ++ bigint_ ++ "add_prime_inplace( uint64_t *tgt ) {"
  , "  return " ++ bigint_ ++ "add_inplace( tgt, " ++ prefix ++ "prime);"
  , "}"
  , ""
  , "// the constant `p + 1`"
  , mkConst nlimbs (prefix ++ "p_plus_1") (thePrime + 1)
  , ""
  , "// adds `p+1` to the input, inplace"
  , "uint8_t " ++ prefix ++ "" ++ bigint_ ++ "add_prime_plus_1_inplace( uint64_t *tgt ) {"
  , "  return " ++ bigint_ ++ "add_inplace( tgt, " ++ prefix ++ "p_plus_1);"
  , "}"
  ]

subPrime :: Params -> Code
subPrime Params{..} = 
  [ "// subtracts the prime p from a bigint"
  , "uint8_t " ++ prefix ++ "" ++ bigint_ ++ "sub_prime( const uint64_t *src, uint64_t *tgt ) {"
  , "  return " ++ bigint_ ++ "sub( src, " ++ prefix ++ "prime, tgt );"
  , "}"
  , ""
  , "// subtracts the prime p from a bigint, inplace"
  , "uint8_t " ++ prefix ++ "" ++ bigint_ ++ "sub_prime_inplace( uint64_t *tgt ) {"
  , "  return " ++ bigint_ ++ "sub_inplace( tgt, " ++ prefix ++ "prime);"
  , "}"
  , ""
  ]

negField :: Params -> Code
negField Params{..} = 
  [ "// negates a field element"
  , "void " ++ prefix ++ "neg( const uint64_t *src, uint64_t *tgt ) {"
  , "  if (" ++ bigint_ ++ "is_zero(src)) {"
  , "    " ++ bigint_ ++ "set_zero(tgt);"
  , "  }"
  , "  else {" 
  , "    // mod (-x) p = p - x" 
  ] ++
  [ "    " ++ index j "tgt" ++ " = " ++ showHex64 (ws!!j) ++ " ;" | j<-[0..nlimbs-1] ] ++
  [ "    " ++ bigint_ ++ "sub_inplace(tgt, src);"
  , "  }"
  , "}"
  , ""
  , "// negates a field element"
  , "void " ++ prefix ++ "neg_inplace( uint64_t *tgt ) {"
  , "  if (" ++ bigint_ ++ "is_zero(tgt)) {"
  , "    return;"
  , "  }"
  , "  else {"
  , "    for(int i=0; i<" ++ show nlimbs ++ "; i++) tgt[i] = ~tgt[i];"
  , "    " ++ prefix ++ bigint_ ++ "add_prime_plus_1_inplace(tgt);"
  , "  }"
  , "}"
  ] 
  where
    ws = toWord64sLE thePrime

addField :: Params -> Code
addField Params{..} = 
  [ "// checks if (x < prime)"
  , "uint8_t " ++ prefix ++ "is_valid( const uint64_t *src ) {"
  ] ++ 
  [ "  if (" ++ index (nlimbs-j-1) "src" ++ " <  " ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") return 1;" ++ "\n" ++
    "  if (" ++ index (nlimbs-j-1) "src" ++ gt j   ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") return 0;"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "return 1;"
  , "}"
  , ""
  , "// if (x >= prime) then (x - prime) else x"
  , "void " ++ prefix ++ "" ++ bigint_ ++ "sub_prime_if_above_inplace( uint64_t *tgt ) {"
  ] ++ 
  [ "  if (" ++ index (nlimbs-j-1) "tgt" ++ " <  " ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") return;" ++ "\n" ++
    "  if (" ++ index (nlimbs-j-1) "tgt" ++ gt j   ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") { " ++ prefix ++ bigint_ ++ "sub_prime_inplace( tgt ); return; }"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "}"
  , ""
  , "// adds two field elements"
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint8_t c = 0;" 
  , "  c = " ++ bigint_ ++ "add( src1, src2, tgt );" 
  ] ++ 
    (if needs_check_carry
      then ["  if (c) { " ++ prefix ++ bigint_ ++ "sub_prime_inplace( tgt ); return; }   // TODO: is this correct ??"]  
      else []) ++
  [ "  " ++ prefix ++ "" ++ bigint_ ++ "sub_prime_if_above_inplace( tgt );"
  , "}"
  , ""
  , "// adds two field elements, inplace"
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint8_t c = 0;" 
  , "  c = " ++ bigint_ ++ "add_inplace( tgt, src2 );" 
  ] ++ 
    (if needs_check_carry
      then ["  if (c) { " ++ prefix ++ bigint_ ++ "sub_prime_inplace( tgt ); return; }   // TODO: is this correct ??"]  
      else []) ++
  [ "  " ++ prefix ++ "" ++ bigint_ ++ "sub_prime_if_above_inplace( tgt );"
  , "}"
  ]
  where
    gt j = if j == nlimbs-1 then " >= " else " >  "
    ws = toWord64sLE thePrime
    needs_check_carry = ws!!(nlimbs-1) >= 0x8000_0000_0000_0000

subField :: Params -> Code
subField Params{..} = 
  [ "// subtracts two field elements"
  , "void " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint8_t b = 0;" 
  , "  b = " ++ bigint_ ++ "sub( src1, src2, tgt );"
  , "  if (b) { " ++ prefix ++ bigint_ ++ "add_prime_inplace( tgt ); }"
  , "}"
  , ""
  , "// subtracts two field elements"
  , "void " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint8_t b = 0;" 
  , "  b = " ++ bigint_ ++ "sub_inplace( tgt, src2 );"
  , "  if (b) { " ++ prefix ++ bigint_ ++ "add_prime_inplace( tgt ); }"
  , "}"
  , ""
  , "// tgt := src - tgt"
  , "void " ++ prefix ++ "sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {"
  , "  uint8_t b = 0;" 
  , "  b = " ++ bigint_ ++ "sub_inplace_reverse( tgt, src1 );"
  , "  if (b) { " ++ prefix ++ bigint_ ++ "add_prime_inplace( tgt ); }"
  , "}"
  ]


mulField :: Params -> Code
mulField Params{..} = 
  [ "// squares a field elements"
  , "void " ++ prefix ++ "sqr( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint64_t prod[" ++ show (2*nlimbs) ++ "];"
  , "  " ++ bigint_ ++ "sqr( src, prod );"
  , "  " ++ prefix ++ "reduce_modp( prod, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sqr_inplace( uint64_t *tgt ) {"
  , "  uint64_t prod[" ++ show (2*nlimbs) ++ "];"
  , "  " ++ bigint_ ++ "sqr( tgt, prod );"
  , "  " ++ prefix ++ "reduce_modp( prod, tgt );"
  , "}"
  , ""
  , "// multiplies two field elements"
  , "void " ++ prefix ++ "mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint64_t prod[" ++ show (2*nlimbs) ++ "];"
  , "  " ++ bigint_ ++ "mul( src1, src2, prod );"
  , "  " ++ prefix ++ "reduce_modp( prod, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "mul_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint64_t prod[" ++ show (2*nlimbs) ++ "];"
  , "  " ++ bigint_ ++ "mul( tgt, src2, prod );"
  , "  " ++ prefix ++ "reduce_modp( prod, tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------
-- * reduction mod p

reduceBigInt :: Params -> Code
reduceBigInt Params{..} = the_constants ++
  [ "// subtracts two big integers made up from `nlimbs+1` limbs"
  , "uint8_t " ++ prefix ++ "bigint_sub_inplace_larger( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint8_t b = 0;" 
  , "  for(int j=0; j<" ++ show (nlimbs+1) ++ "; j++) {"
  , "    b = _subborrow_u64( b, tgt[j], src2[j], tgt+j );"
  , "  }"
  , "  return b;"
  , "}"
  , ""
  , " // reduces a number of size " ++ show (2*nlimbs) ++ " limbs modulo p"
  , " // similar the Barret reduction (?)"
  , "void " ++ prefix ++ "reduce_modp( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint64_t tmp1[" ++ show (nlimbs+1) ++ "];"
  , "  uint64_t tmp2[" ++ show (nlimbs+1) ++ "];"
  , "  for(int k=0; k<" ++ show safe  ++ "; k++) { tgt[k] = src[k]; }"
  , "  for(int k=" ++ show safe ++ "; k<" ++ show    nlimbs  ++ "; k++) { tgt[k] = 0; }"
  , "  for(int m=" ++ show safe ++ "; m<" ++ show (2*nlimbs) ++ "; m++) {"
  , "    " ++ bigint_ ++ "scale( src[m], " ++ prefix ++ "mps_table + " ++ show nlimbs ++ "*m, tmp1 );"
  , "    __uint128_t q = src[m];"
  , "    q = q * " ++ prefix ++ "qps_table[m];    // this is `2^(64m) * src[m] / p` in 64-bit fixed-point form"
  , "    " ++ bigint_ ++ "scale( (uint64_t)(q>>64), " ++ prefix ++ "prime, tmp2 );"
  , "    uint8_t b = " ++ prefix ++ "bigint_sub_inplace_larger( tmp1, tmp2 );"
  , "    if (b) { " ++ prefix ++ bigint_ ++ "add_prime_inplace( tmp1 ); }"
  , "    " ++ prefix ++ "add_inplace( tgt , tmp1);"
  , "  }"
  , "}"
  ]
  where
    p  = thePrime
    ws = toWord64sLE p

    -- the number of limbs which, even multiplied by 2^64 are still strictly less than p
    -- we write the input as a sum `(2^(64m)*z_m)`, we don't have to do anything with
    -- the lowest `safe` limbs 
    safe = div (integerLog2 (p-1) - 64) 64

    mps = [ 2^(64*m) `mod` p  | m<-[0..2*nlimbs-1] ]     -- mp = 2^(64m) modulo p 
    qps = [ (2^64*mp + p-1) `div` p | mp <- mps ]        -- qp = mp/p in 64-bit fixpont representations (ceil)

    the_constants = 
      [ "// table of `ceil(2^64 * (2^(64*m) mod p) / p)`"
      , "static const uint64_t " ++ prefix ++ "qps_table[" ++ show (2*nlimbs) ++ "] = { " 
          ++ intercalate ", " (map (showHex64 . fromInteger) qps) ++ " };"
      , ""
      , "// table of `2^(64*m) mod p`"
      , "static const uint64_t " ++ prefix ++ "mps_table[" ++ show (nlimbs*(2*nlimbs)) ++ "] = { " 
      ] ++
      [ intercalate ", " (map showHex64 $ toWord64sLE' nlimbs mp) ++ ","
      | mp <- mps 
      ] ++
      [ "};"
      , "" 
      ]

--------------------------------------------------------------------------------
-- * exponentiation

powField :: Params -> Code
powField Params{..} = 
  [ "// computes `x^e mod p`"
  , "void " ++ prefix ++ "pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {"
  , "  uint64_t e = exponent;"
  , "  uint64_t sqr[" ++ show nlimbs ++ "];"
  , "  " ++ bigint_ ++ "copy( src, sqr );             // sqr := src"
  , "  " ++ bigint_ ++ "set_one( tgt );                     // tgt := 1"
  , "  while(e!=0) {"
  , "    if (e & 1) { " ++ prefix ++ "mul_inplace(tgt, sqr); }"
  , "    " ++ prefix ++ "mul_inplace(sqr, sqr);"
  , "    e = e >> 1;"
  , "  }"
  , "}"
  , ""
  , "// computes `x^e mod p` (for `e` non-negative bigint)"
  , "void " ++ prefix ++ "pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {"
  , "  uint64_t sqr[" ++ show nlimbs ++ "];"
  , "  " ++ bigint_ ++ "copy( src, sqr );             // sqr := src"
  , "  " ++ bigint_ ++ "set_one( tgt );                     // tgt := 1"
  , "  int s = expo_len - 1;"
  , "  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers"
  , "  for(int i=0; i<=s; i++) {"
  , "    uint64_t e = expo[i];"
  , "    for(int j=0; j<64; j++) {"
  , "      if (e & 1) { " ++ prefix ++ "mul_inplace(tgt, sqr); }"
  , "      " ++ prefix ++ "mul_inplace(sqr, sqr);"
  , "      e = e >> 1;"
  , "    }"
  , "  }"
  , "}"
  ]
 
--------------------------------------------------------------------------------
-- * modular inverse

invField :: Params -> Code
invField Params{..} = 
  [ "// `(p+1) / 2 = (div p 2) + 1`"
  , mkConst nlimbs (prefix ++ "half_p_plus_1") (div (thePrime+1) 2)
  , ""
  , "// multiply by the inverse of 2"
  , "// if the input is of the form `2k` then we just shift right"
  , "// if the input is of the form `2k+1`, then:"
  , "//   (2k+1)/2 = (2k+1+p)/2 = (2k+(p+1))/2 = k + (p+1)/2"
  , "// also the latter addition will never overflow."
  , "//"
  , "void " ++ prefix ++ "div_by_2( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint8_t odd = " ++ bigint_ ++ "shift_right_by_1(src, tgt);"
  , "  if (odd) { " ++ bigint_ ++ "add_inplace(tgt, " ++ prefix ++ "half_p_plus_1); }"
  , "}"
  , ""
  , "void " ++ prefix ++ "div_by_2_inplace( uint64_t *tgt ) {"
  , "  uint8_t odd = " ++ bigint_ ++ "shift_right_by_1(tgt, tgt);"
  , "  if (odd) { " ++ bigint_ ++ "add_inplace(tgt, " ++ prefix ++ "half_p_plus_1); }"
  , "}"
  , ""
  , "// extended binary euclidean algorithm"
  , "void " ++ prefix ++ "euclid( uint64_t *x1, uint64_t *x2, uint64_t *u, uint64_t *v, uint64_t *tgt ) {"
  , ""
  , "  while( ( (!" ++ bigint_ ++ "is_one(u)) && (!" ++ bigint_ ++ "is_one(v)) ) ) {"
  , ""
  , "    // note: x1 < p"
  , "    // if x1 is odd, it can't be p-1, hence, it's at most p-2"
  , "    // then we divide by two: (p-2)/2 = (p-3)/2"
  , "    // (p-3)/2 + (p+1)/2 = (2p-2)/2 = (p-1)"
  , "    // so the addition x1 + (p+1)/2 = (x1+p)/2 will never overflow"
  , ""
  , "    while (!(u[0] & 1)) {"
  , "      " ++ bigint_ ++ "shift_right_by_1(u,u);"
  , "      uint8_t odd = " ++ bigint_ ++ "shift_right_by_1(x1,x1);"
  , "      if (odd) { " ++ bigint_ ++ "add_inplace(x1, " ++ prefix ++ "half_p_plus_1); }"
  , "    }"
  , ""
  , "    while (!(v[0] & 1)) {"
  , "      " ++ bigint_ ++ "shift_right_by_1(v,v);"
  , "      uint8_t odd = " ++ bigint_ ++ "shift_right_by_1(x2,x2);"
  , "      if (odd) { " ++ bigint_ ++ "add_inplace(x2, " ++ prefix ++ "half_p_plus_1); }"
  , "    }"
  , ""
  , "    uint64_t w[" ++ show nlimbs ++ "];"
  , "    uint8_t b = " ++ bigint_ ++ "sub(u,v,w);  // w = u - v "
  , "    if (b) {"
  , "      // u-v < 0, that is, u < v"
  , "      " ++ bigint_ ++ "neg(w,v);              // v  := v  - u"
  , "      " ++ prefix ++ "sub_inplace(x2,x1);     // x2 := x2 - x1"
  , "    }"
  , "    else {"
  , "      // u-v >= 0, that is, u >= v"
  , "      " ++ bigint_ ++ "copy(w,u);             // u  := u  - v"
  , "      " ++ prefix ++ "sub_inplace(x1,x2);     // x1 := x1 - x2"
  , "    }"
  , "  "
  , "  }"
  , ""
  , "  if (" ++ bigint_ ++ "is_one(u)) { "
  , "    " ++ bigint_ ++ "copy( x1, tgt ); "
  , "  } "
  , "  else { "
  , "    " ++ bigint_ ++ "copy( x2, tgt ); "
  , "  }"
  , "}"
  , ""
  , "// inverse of a field element"
  , "void " ++ prefix ++ "inv( const uint64_t *src, uint64_t *tgt ) {"
  , "  if (" ++ bigint_ ++ "is_zero(src)) { "
  , "    " ++ bigint_ ++ "set_zero(tgt); "
  , "  } "
  , "  else {"
  , "    uint64_t x1[" ++ show nlimbs ++ "];"
  , "    uint64_t x2[" ++ show nlimbs ++ "];"
  , "    uint64_t u [" ++ show nlimbs ++ "];"
  , "    uint64_t v [" ++ show nlimbs ++ "];"
  , ""
  , "    " ++ bigint_ ++ "set_one   ( x1 );               // x1 := 1     "
  , "    " ++ bigint_ ++ "set_zero  ( x2 );               // x2 := 0     "
  , "    " ++ bigint_ ++ "copy( src       , u );    // u  := src    "
  , "    " ++ bigint_ ++ "copy( " ++ prefix ++ "prime , v );    // v  := p      "
  , "    "
  , "    " ++ prefix ++ "euclid(x1,x2,u,v,tgt);"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "inv_inplace( uint64_t *tgt ) {"
  , "  " ++ prefix ++ "inv(tgt,tgt);"
  , "}"
  , ""
  , "// division in the field"
  , "void " ++ prefix ++ "div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  if (" ++ bigint_ ++ "is_zero(src2)) { "
  , "    " ++ bigint_ ++ "set_zero(tgt); "
  , "  } "
  , "  else {"
  , "    uint64_t x1[" ++ show nlimbs ++ "];"
  , "    uint64_t x2[" ++ show nlimbs ++ "];"
  , "    uint64_t u [" ++ show nlimbs ++ "];"
  , "    uint64_t v [" ++ show nlimbs ++ "];"
  , ""
  , "    " ++ bigint_ ++ "copy( src1 , x1 );         // x1 := src1  "
  , "    " ++ bigint_ ++ "set_zero  ( x2 );                // x2 := 0     "
  , "    " ++ bigint_ ++ "copy( src2      , u );     // u  := src2  "
  , "    " ++ bigint_ ++ "copy( " ++ prefix ++ "prime , v );     // v  := p     "
  , "    "
  , "    " ++ prefix ++ "euclid(x1,x2,u,v,tgt);"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "div_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  " ++ prefix ++ "div(tgt,src2,tgt);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: Params -> Code
c_code params = concat $ map ("":)
  [ c_begin   params
    --
  , stdIsOne  params
    --
  , addPrime  params
  , subPrime  params
    --
  , negField  params
  , addField  params
  , subField  params
    --
  , mulField     params
  , reduceBigInt params
    --
  , powField   params
  , invField   params
  ]

hs_code :: Params -> Code
hs_code params@(Params{..}) = concat $ map ("":)
  [ hsBegin      params
  , hsMiscTmp
  , hsConvert    params
  , hsFFI        params
  ]

--------------------------------------------------------------------------------

primefield_std_c_codegen :: FilePath -> Params -> IO ()
primefield_std_c_codegen tgtdir params@(Params{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code params

primefield_std_hs_codegen :: FilePath -> Params -> IO ()
primefield_std_hs_codegen tgtdir params@(Params{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hs_path)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code params

--------------------------------------------------------------------------------

{-
test = do
  let params = Params 
        { prefix      = "bn128r_"        -- prefix for C names
        , nlimbs      = 4                -- number of 64-bit limbs
        , thePrime    = bn128_scalar_r   -- the prime
        , bigint_     = "bigint256_"     -- the corresponding bigint prefix, like "bigint256_"
        , c_basename  = "bn128r"         -- name of the @.c@ / @.h@ file (without extension)
        , hs_basename = "BN128r"         -- the name of the @.hs@ file (without extension) and the type too
        , hs_module   = ""               -- the module path
        }

  primefield_std_c_codegen "/Users/bkomuves/zk_lisbon/zk-algebra/tmp/cbits" params
-}
