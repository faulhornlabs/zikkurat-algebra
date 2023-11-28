
-- | Code shared by the standard and Montgomery representations

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Zikkurat.CodeGen.FieldCommon where

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
  [ "#define I_SRC(i)   (src    + (i)*" ++ show nlimbs ++ ")"
  , "#define I_TGT(i)   (tgt    + (i)*" ++ show nlimbs ++ ")"
  , "#define I_PROD(i)  (prods  + (i)*" ++ show nlimbs ++ ")"
  , "#define I_RECIP(i) (recips + (i)*" ++ show nlimbs ++ ")"
  , ""
  , "// computes the inverse of many field elements at the same time, efficiently"
  , "// uses the Montgomery batch inversion trick"
  , "// inverse of a field element"
  , "void " ++ prefix ++ "batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {"
  , "  assert( n >= 1 );"
  , "  uint64_t *prods  = malloc( 8*" ++ show nlimbs ++ "*n );"
  , "  uint64_t *recips = malloc( 8*" ++ show nlimbs ++ "*n );"
  , "  assert( prods  != 0 );"
  , "  assert( recips != 0 );"
  , "  "
  , "  // compute partial products (a[0]*a[1]*...*a[k]) for all k"
  , "  " ++ prefix ++ "copy( I_SRC(0) , I_PROD(0) );"
  , "  for(int i=1; i<n; i++) {"
  , "    " ++ prefix ++ "mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );"
  , "  }"
  , "  "
  , "  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k"
  , "  " ++ prefix ++ "inv( I_PROD(n-1) , I_RECIP(n-1) );"
  , "  for(int i=n-2; i>=0; i--) {"
  , "    " ++ prefix ++ "mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );"
  , "  }"
  , "  "
  , "  // compute the inverses 1/a[k] for all k"
  , "  " ++ prefix ++ "copy( I_RECIP(0) , I_TGT(0) );"
  , "  for(int i=1; i<n; i++) {"
  , "    " ++ prefix ++ "mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );"
  , "  }"
  , "  "
  , "  free(recips);"
  , "  free(prods);"
  , "}"
  ]

--------------------------------------------------------------------------------

ffi_exponentiation :: CommonParams -> [String]
ffi_exponentiation (CommonParams{..}) =
  [ "----------------------------------------"
  , ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "pow_gen\" c_" ++ prefix ++ "pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()"
  , ""
  , "pow :: " ++ typeName ++ " -> Integer -> " ++ typeName 
  , "pow x e"
  , "  | e == 0      = if isZero x then zero else one"
  , "  | e >  0      =      powNonNeg x         e  " 
  , "  | otherwise   = inv (powNonNeg x (negate e))"
  , ""
  , "withInteger :: Integer -> ((Int, Ptr Word64) -> IO a) -> IO a"
  , "withInteger !input action = do"
  , "  let (n,ws) = toWord64sLE_ input"
  , "  allocaArray n $ \\ptr -> do"
  , "    pokeArray ptr ws"
  , "    action (n,ptr)"
  , ""
  , "{-# NOINLINE powNonNeg #-}"
  , "powNonNeg :: " ++ typeName ++ " -> Integer -> " ++ typeName 
  , "powNonNeg (Mk" ++ typeName ++ " fptr1) expo = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show nlimbs
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withInteger expo $ \\(nwords,ptr2) -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)"
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  , "{-# NOINLINE pow #-}"
  , "pow" ++ bigintType ++ " :: " ++ typeName ++ " -> " ++ bigintType ++ " -> " ++ typeName 
  , "pow" ++ bigintType ++ " (Mk" ++ typeName ++ " fptr1) (Mk" ++ bigintType ++ " fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show nlimbs
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_" ++ prefix ++ "pow_gen ptr1 ptr2 ptr3 " ++ show (nlimbs)
  , "  return (Mk" ++ typeName ++ " fptr3)"
  , ""
  ]

ffi_batch_inverse :: CommonParams -> [String]
ffi_batch_inverse (CommonParams{..}) =
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

exportFieldToC :: CommonParams -> [String]
exportFieldToC (CommonParams{..}) =
  [ ""
  , "{-# NOINLINE exportToCDef #-}"
  , "exportToCDef :: String -> " ++ typeName ++ " -> String"
  , "exportToCDef name val = unsafePerformIO $ do"
  , "  ws <- peekFlat val"
  , "  return $ exportWordsToC name ws"
  , ""
  , "{-# NOINLINE exportListToCDef #-}"
  , "exportListToCDef :: String -> [" ++ typeName ++ "] -> String"
  , "exportListToCDef name vals = unsafePerformIO $ do"
  , "  ws <- mapM peekFlat vals"
  , "  return $ exportWordsArrayToC " ++ show nlimbs ++ " name (concat ws)"
  , ""
  ]

--------------------------------------------------------------------------------
