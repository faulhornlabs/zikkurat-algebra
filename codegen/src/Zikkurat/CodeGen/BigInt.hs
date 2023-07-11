
-- | Generate C code for bigints of fixed with 
-- (made up from a few 64-bit limbs)

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Zikkurat.CodeGen.BigInt where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.FFI
import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data Params = Params 
  { prefix      :: String       -- ^ prefix for C names
  , nlimbs      :: Int          -- ^ number of 64-bit limbs
  , c_basename  :: FilePath     -- ^ name of the @.c@ / @.h@ file (without extension)
  , hs_basename :: FilePath     -- ^ the name of the @.hs@ file (without extension) and the type too
  , hs_module   :: String       -- ^ the module path
  }
  deriving Show

--------------------------------------------------------------------------------

c_header :: Params -> Code
c_header (Params{..}) =
  [ "#include <stdint.h>"
  , ""
  , "extern uint8_t  " ++ prefix ++ "is_zero   ( const uint64_t *src );"
  , "extern uint8_t  " ++ prefix ++ "is_one    ( const uint64_t *src );"
  , "extern uint8_t  " ++ prefix ++ "is_equal  ( const uint64_t *src1, const uint64_t *src2 );"
  , "extern void     " ++ prefix ++ "set_zero  (       uint64_t *tgt );"
  , "extern void     " ++ prefix ++ "set_one   (       uint64_t *tgt );"
  , "extern void     " ++ prefix ++ "set_small (       uint64_t *tgt , uint64_t s );"
  , "extern void     " ++ prefix ++ "copy      ( const uint64_t *src , uint64_t *tgt );"
  , ""
  , "extern void     " ++ prefix ++ "print      ( const uint64_t *src, int underscore_separators );"
  , "extern void     " ++ prefix ++ "debug_print( const char *txt, const uint64_t *src );"
  , ""
  , "extern void     " ++ prefix ++ "neg( const uint64_t *src , uint64_t *tgt );"
  , "extern uint8_t  " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern uint8_t  " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void     " ++ prefix ++ "sqr( const uint64_t *src , uint64_t *tgt );"
  , "extern void     " ++ prefix ++ "mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , "extern void     " ++ prefix ++ "mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );"
  , ""
  , "extern void     " ++ prefix ++ "neg_inplace( uint64_t *tgt );"
  , "extern uint8_t  " ++ prefix ++ "inc_inplace( uint64_t *tgt );"
  , "extern uint8_t  " ++ prefix ++ "dec_inplace( uint64_t *tgt );"
  , "extern uint8_t  " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , "extern uint8_t  " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 );"
  , "extern uint8_t  " ++ prefix ++ "sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );"
  , ""
  , "extern void     " ++ prefix ++ "scale( uint64_t z, const uint64_t *src, uint64_t *tgt );"
  , ""
  , "extern uint8_t  " ++ prefix ++ "shift_left_by_1     ( const uint64_t *src, uint64_t *tgt         );"
  , "extern void     " ++ prefix ++ "shift_left_by_k     ( const uint64_t *src, uint64_t *tgt, int by );"
  , "extern uint8_t  " ++ prefix ++ "shift_right_by_1    ( const uint64_t *src, uint64_t *tgt         );"
  , "extern void     " ++ prefix ++ "shift_right_by_k    ( const uint64_t *src, uint64_t *tgt, int by );"
  ]

--------------------------------------------------------------------------------

hsBegin :: Params -> Code
hsBegin (Params{..}) =
  [ "-- NOTE 1: This module is intented to be imported qualified"
  , "-- NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}"
  , "module " ++ hs_module ++ hs_basename 
  , "  ( " ++ typeName ++ "(..)"
  , "    -- * conversion"
  , "  , to" ++ postfix 
  , "  , from" ++ postfix 
  , "    -- * some numbers"
  , "  , small , zero , one , two"
  , "    -- * predicates"  
  , "  , isZero , isOne , isEqual"
  , "    -- * ring operations"  
  , "  , neg , add , sub"
  , "  , sqr , mul"
  , "    -- * shifts"
  , "  , shiftLeft1 , shiftRight1"
  , "  , shiftLeft  , shiftRight"
  , "    -- * extended multiplication"
  , "  , sqrExt"
  , "  , mulExt"
  , "  , scaleExt"
  , "    -- * random"
  , "  , rnd"
  , "  )"
  , "  where" 
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "import Data.Bits"
  , "import Data.Word"
  , ""
  , "import Control.Monad"
  , ""
  , "import Foreign.C"
  , "import Foreign.Ptr"
  , "import Foreign.ForeignPtr"
  , "import Foreign.Marshal"
  , ""
  , "import System.Random"
  , "import System.IO.Unsafe"
  , ""
  , "import ZK.Algebra.BigInt.Types"
  , ""
  , "import qualified ZK.Algebra.Class.Flat  as L"
  , "import qualified ZK.Algebra.Class.Field as C"
  , ""
  , "--------------------------------------------------------------------------------  "
--  , ""
--  , "newtype " ++ typeName    ++ " = Mk" ++    typeName ++ " (ForeignPtr Word64)"
--  , "newtype " ++ dblTypeName ++ " = Mk" ++ dblTypeName ++ " (ForeignPtr Word64)"
--  , "newtype " ++ sclTypeName ++ " = Mk" ++ sclTypeName ++ " (ForeignPtr Word64)"
  , ""
  , "to" ++ postfix ++ " :: Integer -> " ++ typeName
  , "to" ++ postfix ++ " = unsafeTo" ++ postfix
  , ""
  , "from" ++ postfix ++ " :: " ++ typeName ++ " -> Integer" 
  , "from" ++ postfix ++ " = unsafeFrom" ++ postfix
  , ""
  , "zero, one, two :: " ++ typeName
  , "zero = small 0"
  , "one  = small 1"
  , "two  = small 2"
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
  , "  fptr <- mallocForeignPtrArray " ++ show nlimbs
  , "  withForeignPtr fptr $ \\ptr -> do"
  , "    xs <- replicateM " ++ show nlimbs ++ " (randomIO :: IO Word64)"
  , "    pokeArray ptr xs"
  , "  return (Mk" ++ typeName ++ " fptr)"
  , ""
  , "instance C.Rnd " ++ typeName ++ " where"
  , "  rndIO = rnd"
  , ""
  , "instance C.Ring " ++ typeName ++ " where"
  , "  ringNamePxy _ = \"" ++ typeName ++ "\""
  , "  ringSizePxy _ = " ++ show (2^(64*nlimbs)) 
  , "  isZero = isZero"
  , "  isOne  = isOne"
  , "  zero   = zero"
  , "  one    = one"
  , "  power  = C.ringPowerDefault"
  , ""
--  , "----------------------------------------"
--  , ""
--  , "{-# NOINLINE lowerHalf #-}" 
--  , "lowerHalf :: " ++ dblTypeName ++ " -> " ++ typeName 
--  , "lowerHalf (Mk" ++ dblTypeName ++ " fptr1) = unsafePerformIO $ do"
--  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs)
--  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
--  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
--  , "      copyBytes ptr2 ptr1 " ++ show (64*nlimbs)
--  , "  return (Mk" ++ typeName ++ " fptr2)"
--  , ""
--  , "{-# NOINLINE upperHalf #-}" 
--  , "upperHalf :: " ++ dblTypeName ++ " -> " ++ typeName 
--  , "upperHalf (Mk" ++ dblTypeName ++ " fptr1) = unsafePerformIO $ do"
--  , "  fptr2 <- mallocForeignPtrArray " ++ show (nlimbs)
--  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
--  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
--  , "      copyBytes ptr2 (plusPtr ptr1 " ++ show (64*nlimbs) ++ ") " ++ show (64*nlimbs)
--  , "  return (Mk" ++ typeName ++ " fptr2)"
  , "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "sqr\" c_" ++ prefix ++ "sqr :: Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE sqrExt #-}"
  , "sqrExt :: " ++ typeName ++ " -> " ++ dblTypeName 
  , "sqrExt (Mk" ++ typeName ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (2*nlimbs)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "sqr ptr1 ptr2"
  , "  return (Mk" ++ dblTypeName ++ " fptr2)"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "mul\" c_" ++ prefix ++ "mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE mulExt #-}"
  , "mulExt :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ dblTypeName 
  , "mulExt (Mk" ++ typeName ++ " fptr1) (Mk" ++ typeName ++ " fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (2*nlimbs)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "mul ptr1 ptr2 ptr3"
  , "  return (Mk" ++ dblTypeName ++ " fptr3)"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "scale\" c_" ++ prefix ++ "scale :: Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE scaleExt #-}"
  , "scaleExt :: Word64 -> " ++ typeName ++ " -> " ++ sclTypeName 
  , "scaleExt s (Mk" ++ typeName ++ " fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray " ++ show (1+nlimbs)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "scale s ptr1 ptr2"
  , "  return (Mk" ++ sclTypeName ++ " fptr2)"
  , ""
  , "----------------------------------------"
  , ""
  ]
  where 
    postfix     = ""  
    typeName    = "BigInt" ++ show (   64 *nlimbs)
    dblTypeName = "BigInt" ++ show ((2*64)*nlimbs)
    sclTypeName = "BigInt" ++ show (64*(1+nlimbs))

hsConvert :: Params -> Code
hsConvert (Params{..}) = ffiMarshal "" typeName nlimbs where
  typeName = "BigInt" ++ show ((64  )*nlimbs)

hsFFI :: Params -> Code
hsFFI (Params{..}) = catCode $ 
  [ mkffi "isZero"      $ cfun "is_zero"          (CTyp [CArgInPtr                         ] CRetBool)
  , mkffi "isOne"       $ cfun "is_one"           (CTyp [CArgInPtr                         ] CRetBool)
  , mkffi "isEqual"     $ cfun "is_equal"         (CTyp [CArgInPtr , CArgInPtr             ] CRetBool)
    --
  , mkffi "small"       $ cfun "set_small"        (CTyp [CArgOutPtr, CArg64                ] CRetVoid)
    -- 
  , mkffi "neg"         $ cfun "neg"              (CTyp [CArgInPtr            , CArgOutPtr ] CRetVoid)
  , mkffi "add"         $ cfun "add"              (CTyp [CArgInPtr , CArgInPtr, CArgOutPtr ] CRetVoid)
  , mkffi "sub"         $ cfun "sub"              (CTyp [CArgInPtr , CArgInPtr, CArgOutPtr ] CRetVoid)
    --
  , mkffi "sqr"         $ cfun "sqr_truncated"    (CTyp [CArgInPtr            , CArgOutPtr ] CRetVoid)
  , mkffi "mul"         $ cfun "mul_truncated"    (CTyp [CArgInPtr , CArgInPtr, CArgOutPtr ] CRetVoid)
    --
  , mkffi "shiftLeft1"  $ cfun "shift_left_by_1"  (CTyp [CArgInPtr , CArgOutPtr            ] CRetBool)
  , mkffi "shiftRight1" $ cfun "shift_right_by_1" (CTyp [CArgInPtr , CArgOutPtr            ] CRetBool)
    --
  , mkffi "shiftLeft"   $ cfun "shift_left_by_k"  (CTyp [CArgInPtr , CArgOutPtr , CArgInt  ] CRetVoid)
  , mkffi "shiftRight"  $ cfun "shift_right_by_k" (CTyp [CArgInPtr , CArgOutPtr , CArgInt  ] CRetVoid)
  ]
  where
    cfun cname = CFun (prefix ++ cname)
    mkffi = ffiCall hsTyDesc
    hsTyDesc = HsTyDesc 
      { hsTyName =   "BigInt" ++ show (64*nlimbs)
      , hsTyCon  = "MkBigInt" ++ show (64*nlimbs)
      , hsNLimbs = nlimbs
      }

hsTypesModule :: Code
hsTypesModule = 
  [ "module ZK.Algebra.BigInt.Types where"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "import Data.Word"
  , "import Foreign.ForeignPtr"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "newtype BigInt128  = MkBigInt128  (ForeignPtr Word64)"
  , "newtype BigInt192  = MkBigInt192  (ForeignPtr Word64)"
  , "newtype BigInt256  = MkBigInt256  (ForeignPtr Word64)"
  , "newtype BigInt320  = MkBigInt320  (ForeignPtr Word64)"
  , "newtype BigInt384  = MkBigInt384  (ForeignPtr Word64)"
  , "newtype BigInt448  = MkBigInt448  (ForeignPtr Word64)"
  , "newtype BigInt512  = MkBigInt512  (ForeignPtr Word64)"
  , "newtype BigInt576  = MkBigInt576  (ForeignPtr Word64)"
  , "newtype BigInt640  = MkBigInt640  (ForeignPtr Word64)"
  , "newtype BigInt704  = MkBigInt704  (ForeignPtr Word64)"
  , "newtype BigInt768  = MkBigInt768  (ForeignPtr Word64)"
  , "newtype BigInt832  = MkBigInt832  (ForeignPtr Word64)"
  , "newtype BigInt896  = MkBigInt896  (ForeignPtr Word64)"
  , "newtype BigInt960  = MkBigInt960  (ForeignPtr Word64)"
  , "newtype BigInt1024 = MkBigInt1024 (ForeignPtr Word64)"
  , ""
  , "--------------------------------------------------------------------------------"
  ]
  
--------------------------------------------------------------------------------

begin :: Params -> Code
begin (Params{..}) =
  [ "// unsigned big integers composed of " ++ show nlimbs ++ " 64-bit words"
  , "//"
  , "// NOTE: generated code, do not edit!"
  , ""
  , "#include <stdint.h>"
  , "#include <string.h>"
  , "#include <stdio.h>"
  , "#include <x86intrin.h>"
  , "#include <assert.h>"
  , "#include \"" ++ c_basename ++ ".h\""
  , ""
  , "#define NLIMBS " ++ show nlimbs
  , ""
  , "#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )"
  , "#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )"
  , ""
  , "inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {"
  , "  uint8_t c;"
  , "  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );"
  , "  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );"
  , "  return c;"
  , "}"
  , ""
  , "//------------------------------------------------------------------------------"
  ]

--  , "inline uint8_t addcarry_u128( uint64_t lo1, uint64_t hi1, uint64_t lo2, uint64_t hi2, uint64_t *lo3, uint64_t *hi3) {"
--  , "  uint8_t c;"
--  , "  c = _addcarry_u64( 0, lo1, lo2, lo3 );"
--  , "  c = _addcarry_u64( c, hi1, hi2, hi3 );"
--  , "  return c;"
--  , "}"
--  , ""

small :: Params -> Code
small (Params{..}) =
  [ "uint8_t " ++ prefix ++ "is_zero(const uint64_t *src) {"
  , "  return ( " ++ intercalate " && " [ "(" ++ index j "src" ++ " == 0)" | j<-[0..nlimbs-1] ] ++ ") ;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_one(const uint64_t *src) {"
  , "  return ( " ++ intercalate " && " [ "(" ++ index j "src" ++ " == " ++ show y ++ ")" | j<-[0..nlimbs-1], let y = if j==0 then 1 else 0 ] ++ ") ;"
  , "}"
  , ""
  , "uint8_t " ++ prefix ++ "is_equal(const uint64_t *src1, const uint64_t *src2 ) {"
  , "  return ( " ++ intercalate " && " [ "(" ++ index j "src1" ++ " == " ++ index j "src2" ++ ")" | j<-[0..nlimbs-1], let y = if j==0 then 1 else 0 ] ++ ") ;"
  , "}"
  , ""
  , "void " ++ prefix ++ "set_zero(uint64_t *tgt) {"
  , "  memset( tgt, 0, " ++ show (8*nlimbs) ++ " );"
  , "}" 
  , ""
  , "void " ++ prefix ++ "set_one(uint64_t *tgt) {"
  , "  memset( tgt, 0, " ++ show (8*nlimbs) ++ " );"
  , "  tgt[0] = 1;"
  , "}" 
  , ""
  , "void " ++ prefix ++ "set_small(uint64_t *tgt, uint64_t s) {"
  , "  memset( tgt, 0, " ++ show (8*nlimbs) ++ " );"
  , "  tgt[0] = s;"
  , "}" 
  , ""
  , "void " ++ prefix ++ "copy(const uint64_t *src, uint64_t *tgt) {"
  , "  if (src != tgt) { memcpy( tgt, src, " ++ show (8*nlimbs) ++ " ); }"
  , "}" 
  ]

--------------------------------------------------------------------------------

debugPrint :: Params -> Code
debugPrint (Params{..}) = 
  [ "void " ++ prefix ++ "print(const uint64_t *what, int underscore_separators) {"
  , "  if (underscore_separators) {"
  , "    for(int i=0; i<" ++ show nlimbs ++ "; i++) {"
  , "      printf(\"%016llx_\", what[" ++ show (nlimbs-1) ++ "-i]);"
  , "    }"
  , "  }"
  , "  else {"
  , "    for(int i=0; i<" ++ show nlimbs ++ "; i++) {"
  , "      printf(\"%016llx\", what[" ++ show (nlimbs-1) ++ "-i]);"
  , "    }"
  , "  }"
  , "}"
  , ""
  , "void " ++ prefix ++ "debug_print(const char *txt, const uint64_t *what) {"
  , "  printf(\"%s = 0x\",txt);"
  , "  " ++ prefix ++ "print( what, 1 );"
  , "  printf(\"\\n\");"
  , "}"
  ]

incDecBigint :: Params -> Code
incDecBigint (Params{..}) = 
  [ "// increments bigint by 1, inplace"
  , "uint8_t " ++ prefix ++ "inc_inplace( uint64_t *tgt ) {"
  , "  uint8_t c = 0;" 
  ] ++ 
  [ "  c = _addcarry_u64( c, " ++ index j "tgt" ++ ", " ++ (if j==0 then "1" else "0") ++ ", " ++ advance j "tgt" ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return c;"
  , "}"
  , ""
  , "// decrements bigint by 1, inplace"
  , "uint8_t " ++ prefix ++ "dec_inplace( uint64_t *tgt ) {"
  , "  uint8_t b = 0;" 
  ] ++ 
  [ "  b = _subborrow_u64( b, " ++ index j "tgt" ++ ", " ++ (if j==0 then "1" else "0") ++ ", " ++ advance j "tgt" ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return b;"
  , "}"
  ]

negBigInt :: Params -> Code
negBigInt (Params{..}) = 
  [ "// negates a bigint"
  , "void " ++ prefix ++ "neg( const uint64_t *src, uint64_t *tgt ) {"
  , "  uint8_t b = 0;" 
  ] ++
  [ "  b = _subborrow_u64( b, 0, " ++ index j "src" ++ ", " ++advance j "tgt" ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "}"
  , ""
  , "// negates a bigint inplace"
  , "void " ++ prefix ++ "neg_inplace( uint64_t *tgt ) {"
  , "  uint8_t b = 0;" 
  ] ++
  [ "  b = _subborrow_u64( b, 0, " ++ index  j "tgt" ++ ", " ++ advance j "tgt" ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "}"
  ] 

addBigInt :: Params -> Code
addBigInt (Params{..}) = 
  [ "// adds two (unsigned) big integers made up from " ++ show nlimbs ++ " limbs (64-bit words)"
  , "uint8_t " ++ prefix ++ "add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint8_t c = 0;" 
  ] ++
  [ "  c = _addcarry_u64( c, " ++ index j "src1" ++ ", " ++ index j "src2" ++ ",  tgt+" ++ show j ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return c;"
  , "}"
  , ""
  , "// adds two big integers made up from " ++ show nlimbs ++ " limbs (64-bit words)"
  , "uint8_t " ++ prefix ++ "add_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint8_t c = 0;" 
  ] ++
  [ "  c = _addcarry_u64( c, " ++ index j "tgt" ++ ", " ++ index j "src2" ++ ",  tgt+" ++ show j ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return c;"
  , "}"
  ]

subBigInt :: Params -> Code
subBigInt (Params{..}) = 
  [ "// subtracts two (unsigned) big integers made up from " ++ show nlimbs ++ " limbs (64-bit words)"
  , "uint8_t " ++ prefix ++ "sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  uint8_t b = 0;" 
  ] ++
  [ "  b = _subborrow_u64( b, " ++ index j "src1" ++ ", " ++ index j "src2" ++ ",  tgt+" ++ show j ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return b;"
  , "}"
  , ""
  , "// subtracts two big integers made up from " ++ show nlimbs ++ " limbs (64-bit words)"
  , "uint8_t " ++ prefix ++ "sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {"
  , "  uint8_t b = 0;" 
  ] ++
  [ "  b = _subborrow_u64( b, " ++ index j "tgt" ++ ", " ++ index j "src2" ++ ",  tgt+" ++ show j ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return b;"
  , "}"
  , ""
  , "// tgt := src - tgt"
  , "uint8_t " ++ prefix ++ "sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {"
  , "  uint8_t b = 0;" 
  ] ++
  [ "  b = _subborrow_u64( b, " ++ index j "src1" ++ ", " ++ index j "tgt" ++ ",  tgt+" ++ show j ++ " );"
  | j<-[0..nlimbs-1]
  ] ++ 
  [ "  return b;"
  , "}"
  ]

addBigInt_gen :: Params -> Code
addBigInt_gen (Params{..}) = 
  [ "// adds two big integers made up from `nlimbs` limbs"
  , "uint8_t " ++ prefix ++ "add_inplace_gen( uint64_t *tgt, const uint64_t *src2, int nlimbs ) {"
  , "  uint8_t c = 0;" 
  , "  for(int j=0; j<nlimbs; j++) {"
  , "    c = _addcarry_u64( c, tgt[j], src2[j], tgt+j );"
  , "  }"
  , "  return c;"
  , "}"
  , ""
--  , "// adds two big integers made up from `nlimbs` limbs"
--  , "uint8_t " ++ prefix ++ "sub_inplace_gen( uint64_t *tgt, const uint64_t *src2, int nlimbs ) {"
--  , "  uint8_t b = 0;" 
--  , "  for(int j=0; j<nlimbs; j++) {"
--  , "    b = _subborrow_u64( b, tgt[j], src2[j], tgt+j );"
--  , "  }"
--  , "  return b;"
--  , "}"
  ]

--------------------------------------------------------------------------------
-- * multiplication

scaleBigInt :: Params -> Code
scaleBigInt (Params{..}) =
  [ "// multiplies an (unsigned) big integers of " ++ show nlimbs ++ " limbs with a 64-bit word"
  , "// note: `tgt` must have space for " ++ show (nlimbs+1) ++ " limbs!"
  , "void " ++ prefix ++ "scale( uint64_t z, const uint64_t *src, uint64_t *tgt) {"
  , "  uint8_t c;"
  , "  __uint128_t x;"
  , "  uint64_t hi,lo;"
  , "  tgt[0] = 0;"
  ] ++ concat
  [ [ "  // limb # " ++ show m
    , "  x = ((__uint128_t) src[" ++ show m ++ "]) * z;"
    , "  lo = (uint64_t) x;"
    , "  hi = (uint64_t)(x >> 64);"
    , "  c = _addcarry_u64( 0, tgt[" ++ show m ++"], lo, tgt+" ++ show m ++ " );"
    , "  tgt[" ++ show (m+1) ++ "] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`"
    ]
  | m <- [0..nlimbs-1]
  ] ++
  [ "}"
  ]

scaleBigInt_gen :: Params -> Code
scaleBigInt_gen (Params{..}) =
  [ "// multiplies an (unsigned) big integers of `len` limbs with a 64-bit word"
  , "// note: `tgt` must have space for `len+1` limbs!"
  , "void " ++ prefix ++ "scale_gen( uint64_t z, const uint64_t *src, uint64_t *tgt, int len ) {"
  , "  tgt[0] = 0;"
  , "  for(int m=0; m<len; m++) {"
  , "    uint8_t c;"
  , "    __uint128_t x = ((__uint128_t) src[m]) * z;"
  , "    uint64_t hi,lo;"
  , "    lo = (uint64_t) x;"
  , "    hi = (uint64_t)(x >> 64);"
  , "    c = _addcarry_u64( 0, tgt[m], lo , tgt+m );"
  , "    tgt[m+1] = hi + c;"
  , "  }"
  , "}"
  ]

sqrBigInt :: Params -> Code
sqrBigInt (Params{..}) =  
  [ "// squares an (unsigned) big integer of " ++ show nlimbs ++ " limbs"
  , "// note: `tgt` must have space for " ++ show (2*nlimbs) ++ " limbs!"
  , "void " ++ prefix ++ "sqr( const uint64_t *src, uint64_t *tgt ) {"
  , "  __uint128_t prod;" 
  , "  uint64_t prod_hi, prod_lo;"
  , "  uint64_t carry;"
  , "  for(int m=0; m<" ++ show (2*nlimbs) ++ "; m++) { tgt[m] = 0; }"
  ] ++ concat
  [ [ "  // *** m = " ++ show m ++ " ***"
    , "  carry = 0;"
    ] ++ concat
    [ [ "  prod = ((__uint128_t) src[" ++ show i ++ "]) * src[" ++ show j ++"];"
      , "  prod_lo = (uint64_t)(prod      );"
      , "  prod_hi = (uint64_t)(prod >> 64);"
      ] ++ (if i==j
        then [ "  carry += addcarry_u128_inplace( " ++ tgt_lo_hi ++ ", prod_lo, prod_hi );" ]
        else [ "  carry += addcarry_u128_inplace( " ++ tgt_lo_hi ++ ", prod_lo, prod_hi );"
             , "  carry += addcarry_u128_inplace( " ++ tgt_lo_hi ++ ", prod_lo, prod_hi );"
             ])
    | i <- [0..div m 2]
    , let j = m-i
    , i < nlimbs
    , j < nlimbs
    ] ++
    if (m < (2*nlimbs-2)) then [ "  tgt[" ++ show (m+2) ++ "] = carry;" ] else []
  | m <- [0..2*(nlimbs-1)]
  , let tgt_lo_hi =  "tgt+" ++ show m ++ ", tgt+" ++ show (m+1) 
  ] ++
  [ "}"
  ]

mulBigInt :: Params -> Code
mulBigInt (Params{..}) = 
  [ "// multiplies two (unsigned) big integers of " ++ show nlimbs ++ " limbs"
  , "// note: `tgt` must have space for " ++ show (2*nlimbs) ++ " limbs!"
  , "void " ++ prefix ++ "mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  __uint128_t prod;" 
  , "  uint64_t prod_hi, prod_lo;"
  , "  uint64_t carry;"
  , "  for(int m=0; m<" ++ show (2*nlimbs) ++ "; m++) { tgt[m] = 0; }"
  ] ++ concat
  [ [ "  // *** m = " ++ show m ++ " ***"
    , "  carry  = 0;"
    ] ++ concat
    [ [ "  prod = ((__uint128_t) src1[" ++ show i ++ "]) * src2[" ++ show j ++"];"
      , "  prod_lo = (uint64_t)(prod      );"
      , "  prod_hi = (uint64_t)(prod >> 64);"
      , "  carry += addcarry_u128_inplace( " ++ tgt_lo_hi ++ ", prod_lo, prod_hi );"
      ]
    | i <- [0..m]
    , let j = m-i
    , i < nlimbs
    , j < nlimbs
    ] ++
    if (m < (2*nlimbs-2)) then [ "  tgt[" ++ show (m+2) ++ "] = carry;" ] else []
  | m <- [0..2*(nlimbs-1)]
  , let tgt_lo_hi =  "tgt+" ++ show m ++ ", tgt+" ++ show (m+1) 
  ] ++
  [ "}"
  ]

mulBigIntTruncated :: Params -> Code
mulBigIntTruncated (Params{..}) = 
  [ "// multiplies two (unsigned) big integers of " ++ show nlimbs ++ " limbs,"
  , "// and *truncates* the result " ++ show nlimbs ++ " limbs"
  , "// (so this gives the ring of integers modulo 2^" ++ show (64*nlimbs) ++ ")"
  , "void " ++ prefix ++ "mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {"
  , "  __uint128_t prod;" 
  , "  uint64_t prod_hi, prod_lo;"
  , "  uint64_t carry;"
  , "  for(int m=0; m<" ++ show (nlimbs) ++ "; m++) { tgt[m] = 0; }"
  ] ++ concat
  [ [ "  // *** m = " ++ show m ++ " ***"
    , "  carry  = 0;"
    ] ++ concat
    [ [ "  prod = ((__uint128_t) src1[" ++ show i ++ "]) * src2[" ++ show j ++"];"
      , "  prod_lo = (uint64_t)(prod      );"
      ] ++ 
      (if (m < nlimbs - 1)
         then [ "  prod_hi = (uint64_t)(prod >> 64);"
              , "  carry += addcarry_u128_inplace( " ++ tgt_lo_hi ++ ", prod_lo, prod_hi );"
              ]
         else [ "  tgt[" ++ show m ++ "] += prod_lo;"
              ])
    | i <- [0..m]
    , let j = m-i
    , i < nlimbs
    , j < nlimbs
    ] ++
    if (m < (nlimbs-2)) then [ "  tgt[" ++ show (m+2) ++ "] = carry;" ] else []
  | m <- [0..(nlimbs-1)]
  , let tgt_lo_hi =  "tgt+" ++ show m ++ ", tgt+" ++ show (m+1) 
  ] ++
  [ "}"
  ]

mulBigInt_gen :: Params -> Code
mulBigInt_gen (Params{..}) = 
  [ "// multiplies two (unsigned) big integers of `n` limbs"
  , "// note: `tgt` must have space for `2*n` limbs!"
  , "void " ++ prefix ++ "mul_gen( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt, int n ) {"
  , "  int two_n = 2*n;"
  , "  for(int m=0; m<(two_n  ); m++) { tgt[m] = 0; }"
  , "  for(int m=0; m<(two_n-1); m++) {"
  , "    uint64_t carry = 0;"
  , "    int A = MAX(0  , m-n+1);"
  , "    int B = MIN(n-1, m    );"
  , "    for(int i=A; i<=B; i++) {"
  , "      int j = m - i;"
  , "      __uint128_t prod = ((__uint128_t) src1[i]) * src2[j];"
  , "      uint64_t prod_lo = (uint64_t)(prod      );"
  , "      uint64_t prod_hi = (uint64_t)(prod >> 64);"
  , "      carry += addcarry_u128_inplace( tgt+m, tgt+m+1, prod_lo, prod_hi );"
  , "    }"
  , "    if (m < (two_n-2)) { tgt[m+2] = carry; }"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------
-- * shifts

shiftLeft :: Params -> Code
shiftLeft (Params{..}) =  
  [ "// shift left by 1 bit"
  , "uint8_t " ++ prefix ++ "shift_left_by_1( const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t tmp[" ++ show nlimbs ++ "];" 
  ] ++
  [ "  tmp[" ++ show (i) ++ "] = (src[" ++ show (i) ++ "] << 1)" ++
           (if i==0 then "" else " | (src[" ++ show (i-1) ++ "] >> 63)") ++ " ;" 
  | i<-[0..nlimbs-1]
  ] ++ 
  [ "  uint8_t c = src[" ++ show (nlimbs-1) ++ "] >> 63;"
  , "  memcpy( tgt, tmp, " ++ show (8*nlimbs) ++ " );"
  , "  return c;"
  , "}"
  ,""
  , "// shift left by k bits, for 0 <= k < 64"
  , "uint64_t " ++ prefix ++ "shift_left_by_small( const uint64_t *src, uint64_t *tgt, int by ) {"
  , "  assert( (by >= 0) && (by < 64) );"
  , "  uint64_t tmp[" ++ show nlimbs ++ "];" 
  ] ++
  [ "  tmp[" ++ show (i) ++ "] = (src[" ++ show (i) ++ "] << by)" ++
           (if i==0 then "" else " | (src[" ++ show (i-1) ++ "] >> (64-by))") ++ " ;" 
  | i<-[0..nlimbs-1]
  ] ++ 
  [ "  uint64_t c = src[" ++ show (nlimbs-1) ++ "] >> (64-by);"
  , "  memcpy( tgt, tmp, " ++ show (8*nlimbs) ++ " );"
  , "  return c;"
  , "}"
  , ""
  , "// shift left by k bits, for k >= 0"
  , "void " ++ prefix ++ "shift_left_by_k( const uint64_t *src, uint64_t *tgt, int by ) {"
  , "  assert(by >= 0);"
  , "  int small = by % 64;"
  , "  int move  = by / 64;"
  , "  if (by >= " ++ show (64*nlimbs) ++ ") {"
  , "    " ++ prefix ++ "set_zero(tgt);"
  , "  } "
  , "  else {"
  , "    for(int i=0    ; i<move; i++) { tgt[i] = 0; }"
  , "    for(int i=move ; i<" ++ show nlimbs ++ "     ; i++) { tgt[i] = src[i-move]; }"
  , "    " ++ prefix ++ "shift_left_by_small(tgt, tgt, small);"
  , "  }"
  , "}"
  ]
 
shiftRight :: Params -> Code
shiftRight (Params{..}) =  
  [ "// shift right by 1 bit"
  , "uint8_t " ++ prefix ++ "shift_right_by_1( const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t tmp[" ++ show nlimbs ++ "];" 
  ] ++
  [ "  tmp[" ++ show (nlimbs-1-i) ++ "] = (src[" ++ show (nlimbs-i-1) ++ "] >> 1)" ++
           (if i==0 then "" else " | (src[" ++ show (nlimbs-i  ) ++ "] << 63)") ++ " ;" 
  | i<-[0..nlimbs-1]
  ] ++ 
  [ "  uint8_t c = src[0] & 1;"
  , "  memcpy( tgt, tmp, " ++ show (8*nlimbs) ++ " );"
  , "  return c;"
  , "}"
  , ""
  , "// shift right by k bits, for 0 <= k < 64"
  , "uint64_t " ++ prefix ++ "shift_right_by_small( const uint64_t *src, uint64_t *tgt, int by ) {"
  , "  assert( (by >= 0) && (by < 64) );"
  , "  uint64_t tmp[" ++ show nlimbs ++ "];" 
  ] ++
  [ "  tmp[" ++ show (nlimbs-1-i) ++ "] = (src[" ++ show (nlimbs-i-1) ++ "] >> by)" ++
           (if i==0 then "" else " | (src[" ++ show (nlimbs-i)   ++ "] << (64-by))") ++ " ;" 
  | i<-[0..nlimbs-1]
  ] ++ 
  [ "  uint64_t c = src[0] & ((1<<by) - 1);"
  , "  memcpy( tgt, tmp, " ++ show (8*nlimbs) ++ " );"
  , "  return c;"
  , "}"
  , ""
  , "// shift right by k bits, for k >= 0"
  , "void " ++ prefix ++ "shift_right_by_k( const uint64_t *src, uint64_t *tgt, int by ) {"
  , "  assert(by >= 0);"
  , "  int small = by % 64;"
  , "  int move  = by / 64;"
  , "  if (by >= " ++ show (64*nlimbs) ++ ") {"
  , "    " ++ prefix ++ "set_zero(tgt);"
  , "  } "
  , "  else {"
  , "    for(int i=0     ; i<" ++ show nlimbs ++ "-move; i++) { tgt[i] = src[i+move]; }"
  , "    for(int i=" ++ show nlimbs ++ "-move; i<" ++ show nlimbs ++ "     ; i++) { tgt[i] = 0;           }"
  , "    " ++ prefix ++ "shift_right_by_small(tgt, tgt, small);"
  , "  }"
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: Params -> Code
c_code params@(Params{..}) = concat $ map ("":)
  [ begin        params
  , small        params
  , debugPrint   params
    --
  , incDecBigint params
  , negBigInt    params
  , addBigInt    params
  , subBigInt    params
    --
  , scaleBigInt  params
  , sqrBigInt    params
  , mulBigInt    params
  , mulBigIntTruncated  params
    --
  , shiftLeft    params
  , shiftRight   params
  ]

hs_code :: Params -> Code
hs_code params@(Params{..}) = concat $ map ("":)
  [ hsBegin      params
  , hsMiscTmp
  , hsConvert    params
  , hsFFI        params
  ]

--------------------------------------------------------------------------------

bigint_c_codegen :: FilePath -> Params -> IO ()
bigint_c_codegen tgtdir params@(Params{..}) = do

  let fn_h = tgtdir </> (c_basename <.> "h")
  let fn_c = tgtdir </> (c_basename <.> "c")

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code params

bigint_hs_codegen :: FilePath -> Params -> IO ()
bigint_hs_codegen tgtdir params@(Params{..}) = do

  let fn_hs = tgtdir </> (hs_basename <.> "hs")

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code params

--------------------------------------------------------------------------------
