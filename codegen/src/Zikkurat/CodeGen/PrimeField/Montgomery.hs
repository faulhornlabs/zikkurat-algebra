
-- | Prime fields in Montgomery representation

{-# LANGUAGE BangPatterns, NumericUnderscores, RecordWildCards #-}
module Zikkurat.CodeGen.PrimeField.Montgomery where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits
import Data.Maybe

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.FieldCommon
import Zikkurat.CodeGen.Misc
import Zikkurat.CodeGen.FFI
import Zikkurat.Primes -- ( integerLog2 )

--------------------------------------------------------------------------------

data Params = Params 
  { prefix        :: String       -- ^ prefix for C names
  , stdPrefix     :: String       -- ^ perfix for the C names of standard repr. version
  , nlimbs        :: Int          -- ^ number of 64-bit limbs
  , thePrime      :: Integer      -- ^ the prime
  , bigint_       :: String       -- ^ the corresponding bigint prefix, like "bigint256_"
  , c_path        :: Path         -- ^ path of the C module (without extension)
  , hs_path       :: Path         -- ^ path of the Hs module
  , c_path_std    :: Path         -- ^ C path of the std. repr. version
  , hs_path_std   :: Path         -- ^ the module path of the std. repr. version
  , typeName      :: String       -- ^ the name of the haskell type
  , bigintType    :: String       -- ^ the name of the haskell type of the corresponding BigInt
  , fieldName     :: String       -- ^ name of the field
  , primGen       :: Integer      -- ^ the primitive generator
  , fftDomain   :: Maybe (Int,Integer)   -- ^ the largest FFT-friendly subgroup
  }
  deriving Show

toCommonParams :: Params -> CommonParams
toCommonParams (Params{..}) = CommonParams{..}

--------------------------------------------------------------------------------

c_header :: Params -> Code
c_header (Params{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern void " ++ prefix ++ "from_std   ( const uint64_t *src ,       uint64_t *tgt );"
  , "extern void " ++ prefix ++ "to_std     ( const uint64_t *src ,       uint64_t *tgt );"
  , ""
  , "extern uint8_t " ++ prefix ++ "is_valid ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_zero  ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_one   ( const uint64_t *src );"
  , "extern uint8_t " ++ prefix ++ "is_equal ( const uint64_t *src1, const uint64_t *src2 );"
  , "extern void    " ++ prefix ++ "set_zero (       uint64_t *tgt );"
  , "extern void    " ++ prefix ++ "set_one  (       uint64_t *tgt );"
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
  , "extern void " ++ prefix ++ "sub_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "add_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , "extern void " ++ prefix ++ "sqr_inplace ( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "mul_inplace ( uint64_t *tgt , const uint64_t *src2);"
  , "extern void " ++ prefix ++ "inv_inplace ( uint64_t *tgt );"
  , "extern void " ++ prefix ++ "div_inplace ( uint64_t *tgt , const uint64_t *src2 );"
  , ""
  , "extern void " ++ prefix ++ "sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 );"
  , ""
  , "extern void " ++ prefix ++ "batch_inv ( int n, const uint64_t *src, uint64_t *tgt );"
  , ""
  , "extern void " ++ prefix ++ "pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );"
  ]

hsBegin :: Params -> Code
hsBegin params@(Params{..}) =
  [ "-- | Prime field (Montgomery representation) with"
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
  , "  , to"    ++ postfix ++ " , from"    ++ postfix 
  , "  , toStd" ++ postfix ++ " , fromStd" ++ postfix
  , "    -- * Field elements"
  , "  , zero , one , two, primGen"
  , "    -- * Predicates"
  , "  , isValid , isZero , isOne , isEqual"
  , "    -- * Field operations"
  , "  , neg , add , sub"
  , "  , sqr , mul"
  , "  , inv , div , batchInv"
  , "    -- * Exponentiation"
  , "  , pow , pow_"
  ] ++ (if isJust fftDomain 
          then [ "    -- * FFT"
               , "  , fftDomain"
               ]
          else []) ++
  [ "    -- * Random"
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
  , "import qualified " ++ hsModule hs_path_std ++ " as Std"
  , ""
  , "import           ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as C"
  ] ++ (if isJust fftDomain 
         then [ "import qualified ZK.Algebra.Class.FFT as T"
              , "import           ZK.Algebra.Class.FFT hiding (fftDomain)"
              ]
         else []) ++
  [ "import ZK.Algebra.Helpers"
  , ""
  , "--------------------------------------------------------------------------------  "
  , ""
  , "newtype " ++ typeName ++ " = Mk" ++ typeName ++ " (ForeignPtr Word64)"
  , ""
  , "prime :: Integer"
  , "prime = " ++ show thePrime
  , ""
  , "to" ++ postfix ++ " :: Integer -> " ++ typeName
  , "to" ++ postfix ++ " = fromStd" ++ postfix ++ " . Std.to" ++ postfix 
  , ""
  , "from" ++ postfix ++ " :: " ++ typeName ++ " -> Integer"
  , "from" ++ postfix ++ " = Std.from" ++ postfix ++ " . toStd" ++ postfix
  , ""
  , "zero, one, two :: " ++ typeName
  , "zero = to" ++ postfix ++ " 0"
  , "one  = to" ++ postfix ++ " 1"
  , "two  = to" ++ postfix ++ " 2"
  , ""
  , "primGen :: " ++ typeName
  , "primGen = to" ++ postfix ++ " " ++ show primGen
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
  , "  ringNamePxy _ = \"" ++ fieldName ++ " (Montgomery repr.)\""
  , "  ringSizePxy _ = prime"
  , "  isZero = " ++ hsModule hs_path ++ ".isZero"
  , "  isOne  = " ++ hsModule hs_path ++ ".isOne"
  , "  zero   = " ++ hsModule hs_path ++ ".zero"
  , "  one    = " ++ hsModule hs_path ++ ".one"
  , "  square = " ++ hsModule hs_path ++ ".sqr"
  , "  power  = " ++ hsModule hs_path ++ ".pow"
  , "  -- power x e = pow x (B.to (mod e (prime-1)))"
  , ""
  , "instance C.Field " ++ typeName ++ " where"
  , "  charPxy    _ = prime"
  , "  dimPxy     _ = 1"  
  , "  primGenPxy _ = primGen"
  , "  batchInverse = batchInv"
  , ""
  ] ++ (case fftDomain of
         Just (siz,gen) ->
           [ "fftDomain :: FFTSubgroup " ++ typeName 
           , "fftDomain = MkFFTSubgroup gen " ++ show siz ++ " where"
           , "  gen :: " ++ typeName
           , "  gen = to" ++ postfix ++ " " ++ show gen
           , ""
           , "instance FFTField " ++ typeName ++ " where"
           , "  fftDomain = " ++ hsModule hs_path ++ ".fftDomain"
           ]
         Nothing -> []) ++
  [ ""  
  , "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "from_std\" c_" ++ prefix ++ "from_std :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE fromStd" ++ postfix ++ "#-}"
  , "fromStd" ++ postfix ++ " :: Std." ++ typeName ++ " -> " ++ typeName 
  , "fromStd" ++ postfix ++ " (Std.Mk" ++ typeName ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "from_std ptr1 ptr2"
  , "  return (Mk" ++ typeName ++ " fptr2)"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "to_std\" c_" ++ prefix ++ "to_std :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE toStd" ++ postfix ++ "#-}"
  , "toStd" ++ postfix ++ " :: " ++ typeName ++ " -> Std." ++ typeName 
  , "toStd" ++ postfix ++ " (Mk" ++ typeName ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "to_std ptr1 ptr2"
  , "  return (Std.Mk" ++ typeName ++ " fptr2)"
  , ""
  ] ++
  exportFieldToC     (toCommonParams params) ++
  ffi_exponentiation (toCommonParams params) ++
  ffi_batch_inverse  (toCommonParams params) 
  where 
    postfix = ""  

------------------------------

hsConvert :: Params -> Code
hsConvert (Params{..}) = ffiMarshal "" typeName nlimbs 

hsFFI :: Params -> Code
hsFFI (Params{..}) = catCode $ 
  [ mkffi "isValid"     $ cfun' "is_valid"        (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isZero"      $ cfun  "is_zero"         (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isOne"       $ cfun  "is_one"          (CTyp [CArgInPtr                          ] CRetBool)
  , mkffi "isEqual"     $ cfun  "is_equal"        (CTyp [CArgInPtr , CArgInPtr              ] CRetBool)
    --
--  , mkffi "fromStd"     $ cfun "from_std"         (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
--  , mkffi "toStd"       $ cfun "to_std"           (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
    --
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "sqr"         $ cfun "sqr"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "mul"         $ cfun "mul"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
  , mkffi "inv"         $ cfun "inv"              (CTyp [CArgInPtr             , CArgOutPtr ] CRetVoid)
  , mkffi "div"         $ cfun "div"              (CTyp [CArgInPtr , CArgInPtr , CArgOutPtr ] CRetVoid)
    --
  , mkffi "pow_"        $ cfun "pow_uint64"       (CTyp [CArgInPtr , CArg64    , CArgOutPtr ] CRetVoid)
  ]
  where
    cfun_ cname = CFun (bigint_   ++ cname)
    cfun  cname = CFun (prefix    ++ cname)
    cfun' cname = CFun (stdPrefix ++ cname)
    mkffi = ffiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =         typeName
      , hsTyCon  = "Mk" ++ typeName
      , hsNLimbs = nlimbs
      }

--------------------------------------------------------------------------------

c_begin :: Params -> Code
c_begin params@(Params{..}) =
  [ "// finite field arithmetic in Montgomery representation, in the prime field with "
  , "//"
  , "//   p = " ++ show thePrime
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <string.h>"
  , "#include <stdlib.h>"
  , "#include <stdint.h>"
  , "#include <assert.h>"
  , ""
  , "#include \"" ++ pathBaseName c_path     ++ ".h\""
  , "#include \"" ++ pathBaseName c_path_std ++ ".h\""
  , "#include \"bigint" ++ show (64*nlimbs) ++ ".h\""
  , "#include \"platform.h\""
  , ""
  , "#define NLIMBS " ++ show nlimbs
  , ""
  , mkConst nlimbs (prefix ++ "prime") thePrime
  , ""
  , "//------------------------------------------------------------------------------"
  ] 

--------------------------------------------------------------------------------
-- * addition / subtraction 

-- these are the same as the standard representation
-- but replicated here for possibly better optimization opportunities 
-- after all Montgomery repr. operations are most of the operations

addPrime :: Params -> Code
addPrime Params{..} = 
  [ "// adds the prime p to a bigint, inplace"
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
  [ "// subtracts the prime p from a bigint, inplace"
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
  , "    uint64_t tmp[NLIMBS];"
  , "    memcpy(tmp, src, 8*NLIMBS);   // if tgt==src, it would overwrite `src` below..."
  ] ++
  [ "    " ++ index j "tgt" ++ " = " ++ showHex64 (ws!!j) ++ " ;" | j<-[0..nlimbs-1] ] ++
  [ "    " ++ bigint_ ++ "sub_inplace(tgt, tmp);  // src);"
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
  [ "// if (x > prime) then (x - prime) else x"
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

--------------------------------------------------------------------------------

data Montgomery = Montgomery 
  { montP  :: Integer         -- the prime
  , montR  :: Integer         -- eg. 2^192
  , montB  :: Integer         -- always 2^64
  , montQ  :: Word64          -- Q*P+1 mod B == 0
  , montR1 :: Integer         -- R   mod P
  , montR2 :: Integer         -- R^2 mod P
  , montR3 :: Integer         -- R^3 mod P
  }
  deriving Show

powMod :: Integer -> Integer -> Prime -> Integer
powMod base expo p 
  | expo >= 0  = go 1 (mod base p) expo 
  | otherwise  = error "powMod: expecting non-negative exponent"
  where
    go !acc !t  0 = acc
    go !acc !t !e = case e .&. 1 of
      0 -> go       acc       (mod (t*t) p) (shiftR e 1)
      1 -> go (mod (acc*t) p) (mod (t*t) p) (shiftR e 1)

precalcMontgomery :: Prime -> Montgomery
precalcMontgomery p
  | ok         = final
  | otherwise  = error "precalcMontgomery: sanity check failed!"
  where
    k = nlimbsRequired p
    r = 2^(64*k)
    b = 2^64
    
    -- we assume b is a power of two and p is odd
    -- EulerPhi[2^64] == 2^63
    q = powMod (mod (-p) b) (div b 2 - 1) b   

    ok = mod (q*p + 1) b == 0

    r1 = mod  r      p
    r2 = mod (r*r)   p 
    r3 = mod (r*r*r) p

    final = Montgomery
      { montP  = p
      , montR  = r
      , montB  = b
      , montQ  = fromInteger q
      , montR1 = r1
      , montR2 = r2 
      , montR3 = r3
      }

montREDC :: Params -> Code
montREDC Params{..} = 
  [ "// Montgomery constants R, R^2, R^3 mod P"
  , mkConst nlimbs (prefix ++ "R_modp"   ) (montR1 mont)
  , mkConst nlimbs (prefix ++ "R_squared") (montR2 mont)
  , mkConst nlimbs (prefix ++ "R_cubed"  ) (montR3 mont)
  , ""
  , "// Montgomery reduction REDC algorithm"
  , "// based on <https://en.wikipedia.org/wiki/Montgomery_modular_multiplication>"
  , "// T is " ++ show (2*nlimbs+1) ++ " sized bigint in Montgomery representation,"
  , "// and assumed to be < 2^" ++ show (64*nlimbs) ++ "*p"
  , "// WARNING: the value in T which will be overwritten!"
  , "//"
  , "void " ++ prefix ++ "REDC_unsafe( uint64_t *T, uint64_t *tgt ) {"
  , "  T[" ++ show (2*nlimbs) ++ "] = 0;"
  , "  for(int i=0; i<" ++ show nlimbs ++ "; i++) {"
  , "    __uint128_t x;"
  , "    uint64_t c;"
  , "    uint64_t m = T[i] * " ++ showHex64 (montQ mont) ++ ";"
  ] ++ concat
  [ [ "    // j = " ++ show j
    , "    x = ((__uint128_t)m) * " ++ prefix ++ "prime[" ++ show j ++ "] + T[i+" ++ show j ++ "]" ++ (if (j>0) then " + c" else "") ++ ";    // note: cannot overflow in 128 bits"
    , "    c = x >> 64;"
    , "    T[i+" ++ show j ++ "] = (uint64_t) x;"
    ]
  | j <- [0..nlimbs-1]
  ] ++
  [ "    uint8_t d = addcarry_u64( 0 , T[i+" ++ show nlimbs ++ "] , c , T+i+" ++ show nlimbs ++ " );"
  , "    for(int j=" ++ show (nlimbs+1) ++ "; (d>0) && (j<=" ++ show (2*nlimbs) ++ "-i); j++) {"
  , "      d = addcarry_u64( d , T[i+j] , 0 , T+i+j );"
  , "    }"
  , "  }"
  , "  memcpy( tgt, T+" ++ show nlimbs ++ ", " ++ show (nlimbs*8) ++ ");"
  , "  " ++ prefix ++ "" ++ bigint_ ++ "sub_prime_if_above_inplace(tgt);"
  , "}"
  , ""
  , "void " ++ prefix ++ "REDC( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  memcpy( T, src, " ++ show (8*2*nlimbs) ++ " );"
  , "  " ++ prefix ++ "REDC_unsafe ( T, tgt );"
  , "}"
  ]
  where
    mont = precalcMontgomery thePrime

montIsValid :: Params -> Code
montIsValid Params{..} =
  [ "// checks if (x < prime)"
  , "uint8_t " ++ prefix ++ "is_valid( const uint64_t *src ) {"
  ] ++ 
  [ "  if (" ++ index (nlimbs-j-1) "src" ++ " <  " ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") return 1;" ++ "\n" ++
    "  if (" ++ index (nlimbs-j-1) "src" ++ gt j   ++ showHex64 (ws!!(nlimbs-j-1)) ++ ") return 0;"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "return 1;"
  , "}"
  ]
  where
    gt j = if j == nlimbs-1 then " >= " else " >  "
    ws = toWord64sLE thePrime

montIsOne :: Params -> Code
montIsOne Params{..} = 
  [ "uint8_t " ++ prefix ++ "is_zero( const uint64_t *src ) {"
  , "  return " ++ bigint_ ++ "is_zero( src );"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_one( const uint64_t *src ) {"
  , "  return " ++ bigint_ ++ "is_equal( src, " ++ prefix ++ "R_modp );"
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
  , "  " ++ bigint_ ++ "copy( " ++ prefix ++ "R_modp , tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "copy( const uint64_t *src, uint64_t *tgt ) {"
  , "  " ++ bigint_ ++ "copy( src , tgt );"
  , "}"
  ]

--------------------------------------------------------------------------------

{-
montAddSub :: Params -> Code
montAddSub p =
  [ "void " ++ prefix ++ "neg( const uint64_t *src, uint64_t *tgt ) {"
  , "  field_neg( src, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "neg_inplace( uint64_t *tgt ) {"
  , "  field_neg_inplace( tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  field_add( src1, src2, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  field_add_inplace( tgt, src2 );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  field_sub( src1, src2, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  field_sub_inplace( tgt, src2 );"
  , "}"
  ]
-}

montMul :: Params -> Code
montMul Params{..} =
  [ "void " ++ prefix ++ "sqr( const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  " ++ bigint_ ++ "sqr( src, T );"
  , "  " ++ prefix ++ "REDC_unsafe( T, tgt );"
  , "};"
  , ""
  , "void " ++ prefix ++ "sqr_inplace( uint64_t *tgt ) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  " ++ bigint_ ++ "sqr( tgt, T );"
  , "  " ++ prefix ++ "REDC_unsafe( T, tgt );"
  , "};"
  , ""
  , "void " ++ prefix ++ "mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  " ++ bigint_ ++ "mul( src1, src2, T );"
  , "  " ++ prefix ++ "REDC_unsafe( T, tgt );"
  , "};"
  , ""
  , "void " ++ prefix ++ "mul_inplace( uint64_t *tgt, const uint64_t *src2) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  " ++ bigint_ ++ "mul( tgt, src2, T );"
  , "  " ++ prefix ++ "REDC_unsafe( T, tgt );"
  , "};"
  ]

montInv :: Params -> Code
montInv Params{..} =
  [ "void " ++ prefix ++ "inv( const uint64_t *src, uint64_t *tgt) {"
  , "  " ++ stdPrefix ++ "inv( src, tgt );"
  , "  " ++ prefix ++ "mul_inplace( tgt, " ++ prefix ++ "R_cubed );"
  , "};"
  , ""
  , "void " ++ prefix ++ "inv_inplace( uint64_t *tgt ) {"
  , "  " ++ stdPrefix ++ "inv_inplace( tgt );"
  , "  " ++ prefix ++ "mul_inplace( tgt, " ++ prefix ++ "R_cubed );"
  , "};"
  , ""
  , "void " ++ prefix ++ "div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {"
  , "  " ++ stdPrefix ++ "div( src1, src2, tgt );"
  , "  " ++ prefix ++ "mul_inplace( tgt, " ++ prefix ++ "R_squared );"
  , "};"
  , ""
  , "void " ++ prefix ++ "div_inplace( uint64_t *tgt, const uint64_t *src2) {"
  , "  " ++ stdPrefix ++ "div_inplace( tgt, src2 );"
  , "  " ++ prefix ++ "mul_inplace( tgt, " ++ prefix ++ "R_squared );"
  , "};"
  ]

montConvert :: Params -> Code
montConvert Params{..} =
  [ "void " ++ prefix ++ "from_std( const uint64_t *src, uint64_t *tgt) {"
  , "  " ++ prefix ++ "mul( src, " ++ prefix ++ "R_squared, tgt );"
  , "}"
  , ""
  , "void " ++ prefix ++ "to_std( const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t T[" ++ show (2*nlimbs+1) ++ "];"
  , "  memcpy( T, src, " ++ show (8*nlimbs) ++ ");"
  , "  memset( T+" ++ show nlimbs ++ ", 0, " ++ show (8*nlimbs) ++ ");"
  , "  " ++ prefix ++ "REDC_unsafe( T, tgt );"
  , "};"
  ]

--------------------------------------------------------------------------------

c_code :: Params -> Code
c_code params = concat $ map ("":)
  [ c_begin   params
    --
  , addPrime  params
  , subPrime  params
    --
  , negField  params
  , addField  params
  , subField  params
    --
  , montREDC  params
  , montMul   params
  , montInv   params
    --
  , exponentiation (toCommonParams params)
  , batchInverse   (toCommonParams params)
    --
  , montIsValid params
  , montIsOne   params
  , montConvert params
  ]

hs_code :: Params -> Code
hs_code params@(Params{..}) = concat $ map ("":)
  [ hsBegin      params
  -- , hsMiscTmp
  , hsConvert    params
  , hsFFI        params
  ]

--------------------------------------------------------------------------------

primefield_Montgomery_c_codegen :: FilePath -> Params -> IO ()
primefield_Montgomery_c_codegen tgtdir params@(Params{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" c_path)
  let fn_c = tgtdir </> (cFilePath "c" c_path)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code params

primefield_Montgomery_hs_codegen :: FilePath -> Params -> IO ()
primefield_Montgomery_hs_codegen tgtdir params@(Params{..}) = do

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
