
-- | FFT for curve points 
-- (used for converting between coefficient and Lagrange form of a KZG trusted setup)

{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.FFT where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

--import Control.Monad
--import System.FilePath

import Zikkurat.CodeGen.Misc
--import Zikkurat.Primes -- ( integerLog2 )

import Zikkurat.CodeGen.Curve.Params
--import Zikkurat.CodeGen.Curve.CurveFFI

--------------------------------------------------------------------------------

fft_c_header :: CodeGenParams -> Code
fft_c_header (CodeGenParams{..}) =
  [ "extern void " ++ prefix ++ "fft_forward( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt );"
  , "extern void " ++ prefix ++ "fft_inverse( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt );"
  ]

fft_hs_binding :: CodeGenParams -> Code
fft_hs_binding (CodeGenParams{..}) =
  [ ""
  , "foreign import ccall unsafe \"" ++ prefix ++ "fft_inverse\" c_" ++ prefix ++ "fft_inverse :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ prefix ++ "fft_forward\" c_" ++ prefix ++ "fft_forward :: CInt -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE forwardFFT #-}"
  , "-- | Forward FFT for groups (converting @[L_k(tau)]@ points to @[tau^i]@ points)"
  , "forwardFFT :: FFTSubgroup Fr -> FlatArray " ++ typeName ++ " -> FlatArray " ++ typeName
  , "forwardFFT sg (MkFlatArray n fptr2)" 
  , "  | subgroupSize sg /= n   = error \"forwardNTT: subgroup size differs from the array size\""
  , "  | otherwise              = unsafePerformIO $ do"
  , "      fptr3 <- mallocForeignPtrArray (n*" ++ show (3*nlimbs_p) ++ ")"
  , "      L.withFlat (subgroupGen sg) $ \\ptr1 -> do"
  , "        withForeignPtr fptr2 $ \\ptr2 -> do"
  , "          withForeignPtr fptr3 $ \\ptr3 -> do"
  , "            c_" ++ prefix ++ "fft_forward (fromIntegral $ subgroupLogSize sg) ptr1 ptr2 ptr3"
  , "      return (MkFlatArray n fptr3)"
  , "" 
  , "{-# NOINLINE inverseFFT #-}"
  , "-- | Inverse FFT for groups (converting @[tau^i]@ points to @[L_k(tau)]@ points)"
  , "inverseFFT :: FFTSubgroup Fr -> FlatArray " ++ typeName ++ " -> FlatArray " ++ typeName
  , "inverseFFT sg (MkFlatArray n fptr2)" 
  , "  | subgroupSize sg /= n   = error \"inverseNTT: subgroup size differs from the array size\""
  , "  | otherwise              = unsafePerformIO $ do"
  , "      fptr3 <- mallocForeignPtrArray (n*" ++ show (3*nlimbs_p) ++ ")"
  , "      L.withFlat (subgroupGen sg) $ \\ptr1 -> do"
  , "        withForeignPtr fptr2 $ \\ptr2 -> do"
  , "          withForeignPtr fptr3 $ \\ptr3 -> do"
  , "            c_" ++ prefix ++ "fft_inverse (fromIntegral $ subgroupLogSize sg) ptr1 ptr2 ptr3"
  , "      return (MkFlatArray n fptr3)"
  , ""
  ]

c_group_fft :: XCurve -> CodeGenParams -> Code
c_group_fft curve cgparams 
  =  c_group_forward_fft       cgparams
  ++ c_group_inverse_fft curve cgparams

c_group_inverse_fft :: XCurve -> CodeGenParams -> Code
c_group_inverse_fft xcurve (CodeGenParams{..}) = 
  [ ""
  , "// -----------------------------------------------------------------------------"
  , ""
  , "// inverse of 2 (standard repr)"
  , mkConst nlimbs_r (prefix ++ "oneHalf") (toMont half_std)
  , ""
  , "void " ++ prefix ++ "fft_inverse_noalloc(int m, int tgt_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {"
  , ""
  , "  if (m==0) {"
  , "    " ++ prefix ++ "copy( src, tgt );"
  , "    return;"
  , "  }"
  , ""
  , "  if (m==1) {"
  , "    // N = 2"
  , "    " ++ prefix ++ "add( src , src + GRP_NLIMBS , tgt                         );   // x + y"
  , "    " ++ prefix ++ "sub( src , src + GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );   // x - y"
  , "    " ++ prefix ++ "scl_Fr_mont( " ++ prefix ++ "oneHalf , tgt                         , tgt                         );      // (x + y)/2"
  , "    " ++ prefix ++ "scl_Fr_mont( " ++ prefix ++ "oneHalf , tgt + tgt_stride*GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );      // (x - y)/2"
  , "    return;"
  , "  }"
  , ""
  , "  else {"
  , "  "
  , "    int N     = (1<< m   );"
  , "    int halfN = (1<<(m-1));"
  , ""
  , "    uint64_t ginv[NLIMBS_R];"
  , "    " ++ prefix_r ++ "inv( gen , ginv );  // gen^-1"
  , ""
  , "    uint64_t gpow[NLIMBS_R];    "
  , "    " ++ prefix_r ++ "copy(" ++ prefix ++ "oneHalf , gpow);  // 1/2"
  , "    for(int j=0; j<halfN; j++) {"
  , "      " ++ prefix   ++ "add( src +  j* GRP_NLIMBS , src + (j+halfN)*GRP_NLIMBS , buf + j        *GRP_NLIMBS  );    // x + y"
  , "      " ++ prefix   ++ "sub( src +  j* GRP_NLIMBS , src + (j+halfN)*GRP_NLIMBS , buf + (j+halfN)*GRP_NLIMBS  );    // x - y"
  , "      " ++ prefix   ++ "scl_Fr_mont( " ++ prefix ++ "oneHalf , buf + j        *GRP_NLIMBS , buf + j        *GRP_NLIMBS );    // (x + y) /  2"
  , "      " ++ prefix   ++ "scl_Fr_mont( gpow                    , buf + (j+halfN)*GRP_NLIMBS , buf + (j+halfN)*GRP_NLIMBS );    // (x - y) / (2*g^k)"
  , "      " ++ prefix_r ++ "mul_inplace( gpow , ginv );      "
  , "    }"
  , ""
  , "    " ++ prefix_r ++ "sqr( gen, gpow );  // gen^2"
  , "    " ++ prefix ++ "fft_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf                    , buf + N*GRP_NLIMBS , tgt                         );"
  , "    " ++ prefix ++ "fft_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf + halfN*GRP_NLIMBS , buf + N*GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );"
  , ""
  , "  }"
  , "}"
  , " "
  , "// inverse FFT of group elements (convert from [tau^i] to [L_k(tau)]"
  , "// `src` and `tgt` should be `N = 2^m` sized arrays of group elements"
  , "// `gen` should be the generator of the multiplicative subgroup (of the scalar field) sized `N`, in _Montgomery_ representation"
  , "// NOTE: we normalize the results"
  , "void " ++ prefix ++ "fft_inverse(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {"
  , "  int N = (1<<m);"
  , "  uint64_t *buf = malloc( 8*GRP_NLIMBS * (2*N) );"
  , "  assert( buf !=0 );"
  , "  " ++ prefix ++ "fft_inverse_noalloc( m, 1, gen, src, buf, tgt );"
  , "  free(buf);"
  , "  for(int i=0; i<N; i++) { "
  , "    " ++ prefix ++ "normalize_inplace( tgt + i*GRP_NLIMBS );"
  , "  }"
  , "}"
  , ""
  , "// -----------------------------------------------------------------------------"
  , ""
  ]
  where
    prime_r  = curveFr (extractCurve1 xcurve) 
    half_std = div (prime_r + 1) 2                   -- (p+1)/2 = 1/2
    toMont x = mod (2^(64*nlimbs_r) * x) prime_r     -- but we need Montgomery repr!

--------------------------------------------------------------------------------

c_group_forward_fft :: CodeGenParams -> Code
c_group_forward_fft (CodeGenParams{..}) = 
  [ ""
  , "#define GRP_NLIMBS (3*NLIMBS_P)"
  , ""
  , "// -----------------------------------------------------------------------------"
  , ""
  , "void " ++ prefix ++ "fft_forward_noalloc( int m, int src_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {"
  , ""
  , "  if (m==0) {"
  , "    " ++ prefix ++ "copy( src, tgt );"
  , "    return;"
  , "  }"
  , ""
  , "  if (m==1) {"
  , "    // N = 2"
  , "    " ++ prefix ++ "add( src , src + src_stride*GRP_NLIMBS , tgt              );    // x + y"
  , "    " ++ prefix ++ "sub( src , src + src_stride*GRP_NLIMBS , tgt + GRP_NLIMBS );    // x - y"
  , "    return;"
  , "  }"
  , ""
  , "  else {"
  , "  "
  , "    int N     = (1<< m   );"
  , "    int halfN = (1<<(m-1));"
  , ""
  , "    uint64_t gpow[NLIMBS_R];"
  , "    " ++ prefix_r ++ "sqr( gen, gpow );  // gen^2"
  , "    "
  , "    " ++ prefix ++ "fft_forward_noalloc( m-1 , src_stride<<1 , gpow , src                         , buf + N*GRP_NLIMBS , buf                    );"
  , "    " ++ prefix ++ "fft_forward_noalloc( m-1 , src_stride<<1 , gpow , src + src_stride*GRP_NLIMBS , buf + N*GRP_NLIMBS , buf + halfN*GRP_NLIMBS );"
  , ""
  , "    " ++ prefix_r ++ "set_one(gpow);"
  , "    for(int j=0; j<halfN; j++) {"
  , "      " ++ prefix ++ "scl_Fr_mont(gpow , buf + (j+halfN)*GRP_NLIMBS  , tgt +  j *GRP_NLIMBS );  //   g*v[k]"
  , "      " ++ prefix ++ "neg ( tgt +  j       *GRP_NLIMBS ,        tgt + (j+halfN)*GRP_NLIMBS );   // - g*v[k]"
  , "      " ++ prefix ++ "add_inplace( tgt +  j       *GRP_NLIMBS , buf + j*GRP_NLIMBS );           // u[k] + g*v[k]"
  , "      " ++ prefix ++ "add_inplace( tgt + (j+halfN)*GRP_NLIMBS , buf + j*GRP_NLIMBS );           // u[k] - g*v[k]"
  , "      " ++ prefix_r ++ "mul_inplace( gpow , gen );      "
  , "    }"
  , "  }"
  , "}"
  , ""
  , "// forward FFT of group elements (convert from [L_k(tau)] to [tau^i])"
  , "// `src` and `tgt` should be `N = 2^m` sized arrays of group elements"
  , "// `gen` should be the generator of the multiplicative subgroup (of the scalar field) sized `N` (in _Montgomery_ representation)"
  , "// NOTE: we normalize the results"
  , "void " ++ prefix ++ "fft_forward (int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {"
  , "  int N = (1<<m);"
  , "  uint64_t *buf = malloc( 8*GRP_NLIMBS * (2*N) );"
  , "  assert( buf !=0 );"
  , "  " ++ prefix ++ "fft_forward_noalloc( m, 1, gen, src, buf, tgt);"
  , "  free(buf);"
  , "  for(int i=0; i<N; i++) { "
  , "    " ++ prefix ++ "normalize_inplace( tgt + i*GRP_NLIMBS );"
  , "  }"
  , "}"
  , ""
  ]

