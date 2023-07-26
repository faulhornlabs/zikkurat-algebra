
-- | Code shared by the standard and Montgomery representations

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Zikkurat.CodeGen.PrimeField.FieldCommon where

--------------------------------------------------------------------------------

data CommonParams = CommonParams 
  { prefix     :: String
  , nlimbs     :: Int
  , typeName   :: String
  , bigintType :: String
  }
  deriving Show

--------------------------------------------------------------------------------
-- * exponentiation

exponentiation :: CommonParams  -> [String]
exponentiation CommonParams {..} = 
  [ "// computes `x^e mod p`"
  , "void " ++ prefix ++ "pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {"
  , "  uint64_t e = exponent;"
  , "  uint64_t sqr[" ++ show nlimbs ++ "];" 
  , "  " ++ prefix ++ "copy( src, sqr );                 // sqr := src"
  , "  " ++ prefix ++ "set_one( tgt );                   // tgt := 1"
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
  , "  " ++ prefix ++ "copy( src, sqr );                 // sqr := src"
  , "  " ++ prefix ++ "set_one( tgt );                   // tgt := 1"
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

batchInverse :: CommonParams -> [String]
batchInverse CommonParams{..} = 
  [ "#define SRC(i)   (src    + (i)*NLIMBS)"
  , "#define TGT(i)   (tgt    + (i)*NLIMBS)"
  , "#define PROD(i)  (prods  + (i)*NLIMBS)"
  , "#define RECIP(i) (recips + (i)*NLIMBS)"
  , ""
  , "// computes the inverse of many field elements at the same time, efficiently"
  , "// uses the Montgomery batch inversion trick"
  , "// inverse of a field element"
  , "void " ++ prefix ++ "batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {"
  , "  assert( n >= 1 );"
  , "  uint64_t *prods  = malloc( 8*NLIMBS*n );"
  , "  uint64_t *recips = malloc( 8*NLIMBS*n );"
  , "  assert( prods  != 0 );"
  , "  assert( recips != 0 );"
  , "  "
  , "  // compute partial products (a[0]*a[1]*...*a[k]) for all k"
  , "  " ++ prefix ++ "copy( SRC(0) , PROD(0) );"
  , "  for(int i=1; i<n; i++) {"
  , "    " ++ prefix ++ "mul( PROD(i-1) , SRC(i) , PROD(i) );"
  , "  }"
  , "  "
  , "  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k"
  , "  " ++ prefix ++ "inv( PROD(n-1) , RECIP(n-1) );"
  , "  for(int i=n-2; i>=0; i--) {"
  , "    " ++ prefix ++ "mul( RECIP(i+1) , SRC(i+1) , RECIP(i) );"
  , "  }"
  , "  "
  , "  // compute the inverses 1/a[k] for all k"
  , "  " ++ prefix ++ "copy( RECIP(0) , TGT(0) );"
  , "  for(int i=1; i<n; i++) {"
  , "    " ++ prefix ++ "mul( RECIP(i) , PROD(i-1) , TGT(i) );"
  , "  }"
  , "  "
  , "  free(recips);"
  , "  free(prods);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_exponentiation :: CommonParams -> [String]
c_exponentiation (CommonParams{..}) =
  [ "----------------------------------------"
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
  ]

c_batch_inverse :: CommonParams -> [String]
c_batch_inverse (CommonParams{..}) =
  [ "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "batch_inv\" c_" ++ prefix ++ "batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE batchInv #-}"
  , "batchInv :: FlatArray " ++ typeName ++ " -> FlatArray " ++ typeName
  , "batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do"
  , "  fptr2 <- mallocForeignPtrArray (n*" ++ show nlimbs ++ ")"
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      c_" ++ prefix ++ "batch_inv (fromIntegral n) ptr1 ptr2"
  , "  return (MkFlatArray n fptr2)"
  , ""
  , "----------------------------------------"
  , ""
  ]

--------------------------------------------------------------------------------

