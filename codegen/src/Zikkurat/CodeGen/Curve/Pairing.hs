
-- | See:
--
-- * Reza Azarderakhsh et al.: \"Fast Software Implementations of Bilinear Pairings\"
--
-- TODO: cyclotomic subgroup squaring optim for the \"hard\" exponentation
--


{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.Pairing where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

-- import qualified ZK.Algebra.Pure.Field.Class 
-- import qualified ZK.Algebra.Pure.Instances.BN254     as BN254
-- import qualified ZK.Algebra.Pure.Instances.BLS12_381 as BLS12_381

import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data Twist
  = DTwist           -- ^ D-type twist (eg. BN128)
  | MTwist           -- ^ M-type twist (eg. BLS12-381)
  deriving Show

data PairingParams = PairingParams 
  { c_curve        :: String       -- ^ "bn128" or "bls12_381"
  , hs_curve       :: String       -- ^ "BN128" or "BLS12_381"
  , nwords_fp      :: Int
  , twist_type     :: Twist
  }
  deriving Show

pairingParams_BN128 = PairingParams 
  { c_curve        = "bn128"
  , hs_curve       = "BN128"
  , nwords_fp      = 4
  , twist_type     = DTwist
  }

pairingParams_BLS12_381 = PairingParams 
  { c_curve        = "bls12_381"
  , hs_curve       = "BLS12_381"
  , nwords_fp      = 6
  , twist_type     = MTwist
  }

--------------------------------------------------------------------------------

{-
data PairingParams = PairingParams 
  { prefix         :: String       -- ^ prefix for C names (what we are generating)
  , prefix_fp      :: String       -- ^ prefix for C names
  , prefix_fp2     :: String       -- ^ prefix for C names
  , prefix_fp12    :: String       -- ^ prefix for C names
  , prefix_g1_aff  :: String       -- ^ prefix for C names
  , prefix_g1_proj :: String       -- ^ prefix for C names
  , prefix_g2_aff  :: String       -- ^ prefix for C names
  , prefix_g2_proj :: String       -- ^ prefix for C names
  }
-}

--------------------------------------------------------------------------------

c_constants :: PairingParams -> Code
c_constants params@(PairingParams{..}) = case c_curve of
  "bn128" ->
    [ "const uint64_t " ++ c_curve ++ "_pairing_twist_B [8] = { 0x3bf938e377b802a8, 0x020b1b273633535d, 0x26b7edf049755260, 0x2514c6324384a86d, 0x38e7ecccd1dcff67, 0x65f0b37d93ce0d3e, 0xd749d0dd22ac00aa, 0x0141b9ce4a688d4d };"
    , "const uint64_t " ++ c_curve ++ "_pairing_twist_3B[8] = { 0x3baa927cb62e0d6a, 0xd71e7c52d1b664fd, 0x03873e63d95d4664, 0x0e75b5b1082ab8f4, 0xaab7c6667596fe35, 0x31d21a78bb6a27ba, 0x85dd7297680401ff, 0x03c52d6adf39a7e9 };"
    , ""
    , "#define MILLER_LOOP_LENGTH 64"
    , ""
    , "// note: the parameter is (6*x+2) = 0x19d797039be763ba8, which is 65 bits."
    , "// but the loop doesn't use the topmost bit, so it just fits into a 64 bit word"
    , "const uint64_t " ++ c_curve ++ "_miller_loop_param = 0x9d797039be763ba8;"
    , ""
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_inv_w2[48] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x9168c5b062e5ff12, 0x65af5018ad07a2d2, 0x3272d31f197d565e, 0x2c9f210801f7f840, 0x684d4eeef09effcd, 0xcca59129dbef59bf, 0x9d189af460e40038, 0x006b3def6e22d9c4, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_inv_w3[48] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x9168c5b062e5ff12, 0x65af5018ad07a2d2, 0x3272d31f197d565e, 0x2c9f210801f7f840, 0x684d4eeef09effcd, 0xcca59129dbef59bf, 0x9d189af460e40038, 0x006b3def6e22d9c4, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
    ]

  "bls12_381" ->
    [ "const uint64_t " ++ c_curve ++ "_pairing_twist_B [12] = { 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e, 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e };"
    , "const uint64_t " ++ c_curve ++ "_pairing_twist_3B[12] = { 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1, 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1 };"   
    , ""
    , "#define MILLER_LOOP_LENGTH 63"
    , ""
    , "const uint64_t " ++ c_curve ++ "_miller_loop_param = 0xd201000000010000;"
    , ""
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_w2[72] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_w3[72] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
    , ""
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_inv_w2[72] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x1804000000015554, 0x855000053ab00001, 0x633cb57c253c276f, 0x6e22d1ec31ebb502, 0xd3916126f2d14ca2, 0x17fbb8571a006596, 0xa1fafffffffe5557, 0x995bfff976a3fffe, 0x03f41d24d174ceb4, 0xf6547998c1995dbd, 0x778a468f507a6034, 0x020559931f7f8103, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 }; "
    , "const uint64_t " ++ c_curve ++ "_pairing_fp12_inv_w3[72] = "
    , "  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x1804000000015554, 0x855000053ab00001, 0x633cb57c253c276f, 0x6e22d1ec31ebb502, 0xd3916126f2d14ca2, 0x17fbb8571a006596, 0xa1fafffffffe5557, 0x995bfff976a3fffe, 0x03f41d24d174ceb4, 0xf6547998c1995dbd, 0x778a468f507a6034, 0x020559931f7f8103, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 }; "
    ]

----------------------------------------

c_psi_inverse_psi :: PairingParams -> Code
c_psi_inverse_psi params@(PairingParams{..}) = case c_curve of
  "bn128"     -> c_bn128_psi_inverse_psi     params
  "bls12_381" -> c_bls12_381_psi_inverse_psi params

c_bn128_psi_inverse_psi :: PairingParams -> Code
c_bn128_psi_inverse_psi params@(PairingParams{..}) = 
  [ "// TODO: optimize this: (w^2, w^3, w^{-2}, w^{-3} are all just permutations and negations and additions of the coordinates)"
  , ""
  , "// affine mapping from G2 to E(Fp12)"
  , "// psi(x,y) = (w^2*x, w^3*y)"
  , "// note:"
  , "// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))"
  , "// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))"
  , "void " ++ c_curve ++ "_pairing_psi(const uint64_t *src, uint64_t *tgt) {"
  , "  memset( tgt , 0 , 8*(2*NWORDS_FP12) );"
--  , "  // " ++ c_curve ++ "_Fp2_mont_copy( src              , tgt               );    "
--  , "  // " ++ c_curve ++ "_Fp2_mont_copy( src + NWORDS_FP2 , tgt + NWORDS_FP12 );    "
--  , "  // " ++ c_curve ++ "_Fp12_mont_mul_inplace( tgt               , fp12_w2 );"
--  , "  // " ++ c_curve ++ "_Fp12_mont_mul_inplace( tgt + NWORDS_FP12 , fp12_w3 );"
  , "  " ++ c_curve ++ "_Fp2_mont_copy( src                , tgt +                 NWORDS_FP2 );"
  , "  " ++ c_curve ++ "_Fp2_mont_copy( src + NWORDS_FP2   , tgt + NWORDS_FP12 + 4*NWORDS_FP2 );"
  , "}"
  , ""
  , "// affine mapping from E(Fp12) to G2"
  , "void " ++ c_curve ++ "_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t x[NWORDS_FP12];"
  , "  uint64_t y[NWORDS_FP12];"
  , "  " ++ c_curve ++ "_Fp12_mont_mul( src               , " ++ c_curve ++ "_pairing_fp12_inv_w2 , x );"
  , "  " ++ c_curve ++ "_Fp12_mont_mul( src + NWORDS_FP12 , " ++ c_curve ++ "_pairing_fp12_inv_w3 , y );"
  , "  // note: if everything is done correctly, the remaining 5 Fp2 coordinates of x/y are zero"
  , "  " ++ c_curve ++ "_Fp2_mont_copy( x , tgt              );    "
  , "  " ++ c_curve ++ "_Fp2_mont_copy( y , tgt + NWORDS_FP2 );    "
  , "}"
  ]

c_bls12_381_psi_inverse_psi :: PairingParams -> Code
c_bls12_381_psi_inverse_psi params@(PairingParams{..}) = 
  [ "// TODO: optimize this: (w^2, w^3, w^{-2}, w^{-3} are all just permutations and negations and additions of the coordinates)"
  , ""
  , "// affine mapping from G2 to E(Fp12)"
  , "// psi(x,y) = (w^{-2}*x, w^{-3}*y)"
  , "// w^{-2} = (((0,0),(0,0),(_,_)),((0,0),(0,0),(0,0)))"
  , "// w^{-3} = (((0,0),(0,0),(0,0)),((0,0),(_,_),(0,0)))"
  , "// note:"
  , "void " ++ c_curve ++ "_pairing_psi(const uint64_t *src, uint64_t *tgt) {"
  , "  memset( tgt , 0 , 8*(2*NWORDS_FP12) );"
  , "  " ++ c_curve ++ "_Fp2_mont_mul( src              , " ++ c_curve ++ "_pairing_fp12_inv_w2 + 2*NWORDS_FP2 , tgt +               2*NWORDS_FP2 );"
  , "  " ++ c_curve ++ "_Fp2_mont_mul( src + NWORDS_FP2 , " ++ c_curve ++ "_pairing_fp12_inv_w3 + 4*NWORDS_FP2 , tgt + NWORDS_FP12 + 4*NWORDS_FP2 );"
  , "}"
  , ""
  , "// affine mapping from E(Fp12) to G2"
  , "// psi^{-1}(x,y) = (w^2*x, w^3*y)"
  , "// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))"
  , "// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))"
  , "void " ++ c_curve ++ "_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t x[NWORDS_FP12];"
  , "  uint64_t y[NWORDS_FP12];"
  , "  " ++ c_curve ++ "_Fp12_mont_mul( src               , " ++ c_curve ++ "_pairing_fp12_w2 , x );"
  , "  " ++ c_curve ++ "_Fp12_mont_mul( src + NWORDS_FP12 , " ++ c_curve ++ "_pairing_fp12_w3 , y );"
  , "  // note: if everything is done correctly, the remaining 5 Fp2 coordinates of x/y are zero"
  , "  " ++ c_curve ++ "_Fp2_mont_copy( x , tgt              );    "
  , "  " ++ c_curve ++ "_Fp2_mont_copy( y , tgt + NWORDS_FP2 );    "
  , "}"
  ]

--------------------------------------------------------------------------------

c_field_structure :: PairingParams -> Code
c_field_structure params@(PairingParams{..}) = case c_curve of
  "bn128" -> 
    [ ""
    , "// used when computing the line function, D-type twist"
    , "// A,B,C in Fp2"
    , "// calculates tgt = A + B*w + C*w^3 in Fp12"
    , "// note:"
    , "// w   = (((0,0),(0,0),(0,0)),((1,0),(0,0),(0,0)))"
    , "// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))"
    , "void " ++ c_curve ++ "_pairing_combine_1_w_w3(const uint64_t *A, const uint64_t* B, const uint64_t *C, uint64_t *tgt) {"
    , "  memset( tgt , 0 , 8*NWORDS_FP12 );"
    , "  memcpy( tgt                 , A , 8*NWORDS_FP2 );"
    , "  memcpy( tgt + 3*NWORDS_FP2  , B , 8*NWORDS_FP2 );"
    , "  memcpy( tgt + 4*NWORDS_FP2  , C , 8*NWORDS_FP2 );"
    , "}"
    ]
  "bls12_381" ->
    [ ""
    , "// used when computing the line function, M-type twist"
    , "// C,B,A in Fp2"
    , "// calculates tgt = C*w^3 + B*w^2 + A in Fp12"
    , "// note:"
    , "// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))"
    , "// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))"
    , "void " ++ c_curve ++ "_pairing_combine_w3_w2_1(const uint64_t *C, const uint64_t* B, const uint64_t *A, uint64_t *tgt) {"
    , "  memset( tgt , 0 , 8*NWORDS_FP12 );"
    , "  memcpy( tgt + 4*NWORDS_FP2  , C , 8*NWORDS_FP2 );"
    , "  memcpy( tgt +   NWORDS_FP2  , B , 8*NWORDS_FP2 );"
    , "  memcpy( tgt                 , A , 8*NWORDS_FP2 );"
    , "}"
    ]

--------------------------------------------------------------------------------

c_hard_expo :: PairingParams -> Code
c_hard_expo params@(PairingParams{..}) = case c_curve of
  "bn128"     -> c_bn128_hard_expo     params
  "bls12_381" -> c_bls12_381_hard_expo params

c_bn128_hard_expo :: PairingParams -> Code
c_bn128_hard_expo params@(PairingParams{..}) =
  [ ""
  , "const uint64_t bn128_pairing_p_minus_lam0[4]                = { 0xb687f7e0078302b6, 0x3a97459a6afe5ea2, 0xb3c4d79d41a91759, 0x0000000000000000 };"
  , "const uint64_t bn128_pairing_lam1_minus_lam0_minus_2lam2[4] = { 0x9d797039be763ba8, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000 };"
  , "const uint64_t bn128_pairing_lam2[4]                        = { 0xf83e9682e87cfd46, 0x6f4d8248eeb859fb, 0x0000000000000000, 0x0000000000000000 };"
  , ""
  , "void bn128_pairing_hard_expo(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t A0[NWORDS_FP12]; "
  , "  uint64_t A1[NWORDS_FP12]; "
  , "  uint64_t A2[NWORDS_FP12]; "
  , "  uint64_t A3[NWORDS_FP12]; "
  , ""
  , "  // --- do the exponentiations ---"
  , ""
  , "  uint64_t running[NWORDS_FP12];"
  , "  bn128_Fp12_mont_copy( src, running );"
  , ""
  , "  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_p_minus_lam0                , A0 , 3 );"
  , "  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_lam1_minus_lam0_minus_2lam2 , A1 , 2 );"
  , "  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_lam2                        , A2 , 2 );"
  , ""
  , "  bn128_Fp12_mont_set_one( A0 );"
  , "  bn128_Fp12_mont_set_one( A1 );"
  , "  bn128_Fp12_mont_set_one( A2 );"
  , ""
  , "  // TODO: optimal multi-chain"
  , "  for(int k=0; k<192; k++) {"
  , "    int i = (k & 63);"
  , "    int j = (k >> 6);"
  , "    if ((bn128_pairing_p_minus_lam0[j]                >> i) & 1) bn128_Fp12_mont_mul_inplace( A0 , running );"
  , "    if ((bn128_pairing_lam1_minus_lam0_minus_2lam2[j] >> i) & 1) bn128_Fp12_mont_mul_inplace( A1 , running );"
  , "    if ((bn128_pairing_lam2[j]                        >> i) & 1) bn128_Fp12_mont_mul_inplace( A2 , running );"
  , "    bn128_Fp12_mont_sqr_inplace( running );"
  , "  }"
  , ""
  , "  // --- combine the final results"
  , ""
  , "  // a0 = frobenius x0 / power x0 q_lam0"
  , "  // a1 = a0 * a2 * a2 * power x0 lam1_lam0_2lam2"
  , "  // a2 =                power x0 lam2"
  , "  // a3 =                      x0"
  , ""
  , "  bn128_Fp12_mont_copy         ( src, A3 );     // x0"
  , "  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob(x0)"
  , ""
  , "  bn128_Fp12_mont_inv_inplace( A0 );"
  , "  bn128_Fp12_mont_mul_inplace( A0, A3 );        // x0^p / x^(p-lam0) = x0^lam0"
  , ""
  , "  bn128_Fp12_mont_mul_inplace( A1, A0 );   "
  , "  bn128_Fp12_mont_mul_inplace( A1, A2 );   "
  , "  bn128_Fp12_mont_mul_inplace( A1, A2 );        // x0^(lam1-lam0-2*lam2) * a0 * a2^2 = x0^lam1"
  , ""
  , "  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob^2(x0)"
  , "  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob^3(x0)"
  , ""
  , "  bn128_Fp12_mont_frobenius_inplace ( A2 );     // frob  (x0^lam2)"
  , "  bn128_Fp12_mont_frobenius_inplace ( A2 );     // frob^2(x0^lam2)"
  , ""
  , "  bn128_Fp12_mont_frobenius_inplace ( A1 );     // frob  (x0^lam1)"
  , ""
  , "  // result = A0*A1*A2*A3"
  , "  bn128_Fp12_mont_mul        (A0,A1,tgt);"
  , "  bn128_Fp12_mont_mul_inplace(tgt,A2);"
  , "  bn128_Fp12_mont_mul_inplace(tgt,A3);"
  , "}"
  ]

----------------------------------------

c_bls12_381_hard_expo :: PairingParams -> Code
c_bls12_381_hard_expo params@(PairingParams{..}) =
  [ ""
  , "const uint64_t bls12_381_pairing_lam2_lam0[6] = { 0x73fefffeaaa9ffff, 0x7efb5555d8a7cffd, 0xd1bb89fe01c38e69, 0x6cd40a3c157b538a, 0x1fb322654a7cef70, 0x0000000000000000 };"
  , "const uint64_t bls12_381_pairing_lam1[6]      = { 0x73ffffffffff5554, 0x9d586d584eacaaaa, 0xc49f25e1a737f5e2, 0x26a48d1bb889d46d, 0x0000000000000000, 0x0000000000000000 };"
  , "const uint64_t bls12_381_pairing_p_lam2[6]    = { 0x9b560000aaab0000, 0x6c2f6d56d2021801, 0x2f1b4444d201019b, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
  , "const uint64_t bls12_381_pairing_lam3[6]      = { 0x8c00aaab0000aaaa, 0x396c8c005555e156, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };"
  , ""
  , "void bls12_381_pairing_hard_expo(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t A0[NWORDS_FP12]; "
  , "  uint64_t A1[NWORDS_FP12]; "
  , "  uint64_t A2[NWORDS_FP12]; "
  , "  uint64_t A3[NWORDS_FP12]; "
  , "  uint64_t frob_x0[NWORDS_FP12]; "
  , ""
  , "  // --- do the exponentiations ---"
  , ""
  , "  uint64_t running[NWORDS_FP12];"
  , "  bls12_381_Fp12_mont_copy( src, running );"
  , ""
  , "  bls12_381_Fp12_mont_set_one( A0 );"
  , "  bls12_381_Fp12_mont_set_one( A1 );"
  , "  bls12_381_Fp12_mont_set_one( A2 );"
  , "  bls12_381_Fp12_mont_set_one( A3 );"
  , ""
  , "  // TODO: optimal multi-chain"
  , "  for(int k=0; k<320; k++) {"
  , "    int i = (k & 63);"
  , "    int j = (k >> 6);"
  , "    if ((bls12_381_pairing_lam2_lam0[j]  >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A0 , running );"
  , "    if ((bls12_381_pairing_lam1[j]       >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A1 , running );"
  , "    if ((bls12_381_pairing_p_lam2[j]     >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A2 , running );"
  , "    if ((bls12_381_pairing_lam3[j]       >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A3 , running );"
  , "    bls12_381_Fp12_mont_sqr_inplace( running );"
  , "  }"
  , ""
  , "  // --- combine the final results"
  , ""
  , "  // a0 = a2           / power x0 lam2_lam0"
  , "  // a1 =                power x0 lam1"
  , "  // a2 = frobenius x0 / power x0 p_lam2"
  , "  // a3 =                power x0 lam3"
  , ""
  , "  bls12_381_Fp12_mont_frobenius( src, frob_x0 );     // frob(x0)"
  , ""
  , "  bls12_381_Fp12_mont_inv_inplace( A0 );   "
  , "  bls12_381_Fp12_mont_inv_inplace( A2 );   "
  , ""
  , "  bls12_381_Fp12_mont_mul_inplace( A2 , frob_x0 );        // x0^p / x^(p-lam2) = x0^lam2"
  , "  bls12_381_Fp12_mont_mul_inplace( A0 , A2      );        // x0^lam2 / x^(lam2-lam0) = x0^lam0"
  , ""
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob  (x0^lam3)"
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob^2(x0^lam3)"
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob^3(x0^lam3)"
  , ""
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A2 );     // frob  (x0^lam2)"
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A2 );     // frob^2(x0^lam2)"
  , ""
  , "  bls12_381_Fp12_mont_frobenius_inplace ( A1 );     // frob  (x0^lam1)"
  , ""
  , "  // result = A0*A1*A2*A3"
  , "  bls12_381_Fp12_mont_mul        (A0,A1,tgt);"
  , "  bls12_381_Fp12_mont_mul_inplace(tgt,A2);"
  , "  bls12_381_Fp12_mont_mul_inplace(tgt,A3);"
  , ""
  , "}"
  ]

--------------------------------------------------------------------------------

c_code :: PairingParams -> Code
c_code params@(PairingParams{..}) =
  [ "#include \"stdint.h\""
  , "#include \"stdlib.h\""
  , "#include \"string.h\""
  , "#include \"assert.h\""
  , ""
  , "#include \"curves/fields/mont/" ++ c_curve ++ "_Fp_mont.h\""
  , "#include \"curves/fields/mont/" ++ c_curve ++ "_Fp2_mont.h\""
  , "#include \"curves/fields/mont/" ++ c_curve ++ "_Fp12_mont.h\""
  , ""
  , "#include \"curves/g1/affine/" ++ c_curve ++ "_G1_affine.h\""
  , "#include \"curves/g2/affine/" ++ c_curve ++ "_G2_affine.h\""
  , "#include \"curves/g1/proj/" ++ c_curve ++ "_G1_proj.h\""
  , "#include \"curves/g2/proj/" ++ c_curve ++ "_G2_proj.h\""
  , ""
  , "//------------------------------------------------------------------------------"
  , ""
  , "#define NWORDS_FP   " ++ show (   nwords_fp)
  , "#define NWORDS_FP2  " ++ show ( 2*nwords_fp)
  , "#define NWORDS_FP12 " ++ show (12*nwords_fp)
  , ""
  , "//------------------------------------------------------------------------------"
  , ""
  ] ++
  c_constants params ++
  [ ""
  , "//------------------------------------------------------------------------------"
  , ""
  ] ++
  c_psi_inverse_psi params ++
  [ ""
  , "void " ++ c_curve ++ "_pairing_frobenius_G2(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t tmp[NWORDS_FP12*2];"
  , "  " ++ c_curve ++ "_pairing_psi(src, tmp);"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace( tmp               );"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace( tmp + NWORDS_FP12 );"
  , "  " ++ c_curve ++ "_pairing_inverse_psi(tmp, tgt);"
  , "}"
  , ""
  , "//------------------------------------------------------------------------------"
  ] ++
  c_field_structure params ++
  [ ""
  , "//------------------------------------------------------------------------------"
  , "// see \"Fast Software Implementations of Bilinear Pairings\" for the addition w/ line formulas"
  , ""
  , "#define Px (P             )"
  , "#define Py (P + NWORDS_FP )"
  , ""
  , "#define Qx (Q             )"
  , "#define Qy (Q + NWORDS_FP2)"
  , ""
  , "#define Tx (T               )"
  , "#define Ty (T +   NWORDS_FP2)"
  , "#define Tz (T + 2*NWORDS_FP2)"
  , ""
  , "//--------------------------------------"
  , ""
  , "// this doubles `T in G2`, and also computes the line function l_psi(2T)(P) in Fp12"
  , "// P should be an affine point in G1, and T projective in G2 !"
  , "void " ++ c_curve ++ "_pairing_miller_double(const uint64_t *P, uint64_t *T, uint64_t *line) {"
  , "  uint64_t A[NWORDS_FP2];"
  , "  uint64_t B[NWORDS_FP2];"
  , "  uint64_t C[NWORDS_FP2];"
  , "  uint64_t D[NWORDS_FP2];"
  , "  uint64_t E[NWORDS_FP2];"
  , "  uint64_t F[NWORDS_FP2];"
  , "  uint64_t G[NWORDS_FP2];"
  , "  uint64_t H[NWORDS_FP2];"
  , "  uint64_t tmp12[NWORDS_FP12];"
  , ""
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Tx, Ty, A);"
  , "  " ++ c_curve ++ "_Fp2_mont_div_by_2_inplace(A);                   // A = (X*Y)/2"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(Ty, B);                            // B = Y^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(Tz, C);                            // C = Z^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(Tx, D);                            // D = X^2"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(C , " ++ c_curve ++ "_pairing_twist_3B, E);    // E = (3*b)*C"
  , "  " ++ c_curve ++ "_Fp2_mont_add(E , E , F);                      "
  , "  " ++ c_curve ++ "_Fp2_mont_add_inplace(F , E);                    // F = 3*E           "
  , "  " ++ c_curve ++ "_Fp2_mont_sub(B , F , Tx);                       //         B - F     "
  , "  " ++ c_curve ++ "_Fp2_mont_mul_inplace(Tx, A);                    // X3 = A*(B - F)"
  , "  " ++ c_curve ++ "_Fp2_mont_add(Ty , Tz , H);                      //      Y + Z"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr_inplace(H);                       //     (Y + Z)^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(H, B);                    //     (Y + Z)^2 - B"
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(H, C);                    // H = (Y + Z)^2 - (B+C)"
  , "  " ++ c_curve ++ "_Fp2_mont_add(B , F , G);                      "
  , "  " ++ c_curve ++ "_Fp2_mont_div_by_2_inplace(G);                  // G = (B+F)/2 "
  , "  " ++ c_curve ++ "_Fp2_mont_mul(B , H , Tz);                      // Z3 = B*H"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(G , A);             //      G^2      "
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(E , C);             //      E^2  "
  , "  " ++ c_curve ++ "_Fp2_mont_sub(A , C, Ty);         //      G^2 - E^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(Ty,C);      //      G^2 - 2*E^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(Ty,C);      // Y3 = G^2 - 3*E^2"
  , ""
  , "  " ++ c_curve ++ "_Fp_mont_add(Px, Px, tmp12);                  // 2*X_p"
  , "  " ++ c_curve ++ "_Fp_mont_add_inplace(tmp12, Px);              // 3*X_p"
  , "  " ++ c_curve ++ "_Fp2_mont_scale_by_prime_field(tmp12, D, C);  // C = 3*X^2*X_p"
  , ""
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(E,B);                   // E - B"
  , ""
  , "  " ++ c_curve ++ "_Fp2_mont_scale_by_prime_field(Py, H, H);     // Y_p*H"
  , "  " ++ c_curve ++ "_Fp2_mont_neg_inplace(H);                     // -Y_p*H"
  , ""
  ] ++   
  (case twist_type of
    DTwist -> [ "  " ++ c_curve ++ "_pairing_combine_1_w_w3 (H,C,E,line);    // -H*Y_p     + 3*X^2*X_p*w   + (E-B)*w^3" ]
    MTwist -> [ "  " ++ c_curve ++ "_pairing_combine_w3_w2_1(H,C,E,line);    // -H*Y_p*w^3 + 3*X^2*X_p*w^2 + (E-B)    " ]
  ) ++
  [ "}"
  , ""
  , "//--------------------------------------"
  , ""
  , "// this computes `T+=Q` in G2, and also computes the line function l_(T+Q)(P) in Fp12"
  , "// NOTE: T is projective, but P (in G1) and Q (in G2) are affine!"
  , "void " ++ c_curve ++ "_pairing_miller_mixed_add(const uint64_t *P, const uint64_t *Q, uint64_t *T, uint64_t *line) {"
  , "  uint64_t A[NWORDS_FP2];"
  , "  uint64_t B[NWORDS_FP2];"
  , "  uint64_t C[NWORDS_FP2];"
  , "  uint64_t D[NWORDS_FP2];"
  , "  uint64_t E[NWORDS_FP2];"
  , "  uint64_t F[NWORDS_FP2];"
  , "  uint64_t G[NWORDS_FP2];"
  , "  uint64_t H[NWORDS_FP2];"
  , "  uint64_t I[NWORDS_FP2];"
  , "  uint64_t J[NWORDS_FP2];"
  , "  uint64_t lambda[NWORDS_FP2];"
  , "  uint64_t theta [NWORDS_FP2];"
  , "  uint64_t tmp12[NWORDS_FP12];"
  , ""
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Qy,Tz,A);             // A = Y2 * Z"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Qx,Tz,B);             // B = X2 * Z"
  , "  if ( " ++ c_curve ++ "_Fp2_mont_is_equal(A,Ty) && " ++ c_curve ++ "_Fp2_mont_is_equal(B,Tx) ) { "
  , "    // Q = T"
  , "    " ++ c_curve ++ "_pairing_miller_double(P,T,line);"
  , "    return;"
  , "  }"
  , ""
  , "  // Q and T are different points"
  , "  " ++ c_curve ++ "_Fp2_mont_sub(Ty,A,theta );         // theta  = Y - A"
  , "  " ++ c_curve ++ "_Fp2_mont_sub(Tx,B,lambda);         // lambda = X - B"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(theta, C);            // C = theta^2"
  , "  " ++ c_curve ++ "_Fp2_mont_sqr(lambda,D);            // D = lambda^2"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(D,lambda,E);          // E = lambda^3"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Tz,C,F);              // F = Z*C"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Tx,D,G);              // G = X*D"
  , "  " ++ c_curve ++ "_Fp2_mont_add(E,F,H);               // H = E + F ..."
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(H,G);         // H = E + F - G..."
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(H,G);         // H = E + F - 2G"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(lambda,H,Tx);         // X3 = lambda*H"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(Ty,E,I);              // I  = Y*E"
  , "  " ++ c_curve ++ "_Fp2_mont_sub(G,H,Ty);              // G - H ..."
  , "  " ++ c_curve ++ "_Fp2_mont_mul_inplace(Ty,theta);    // theta*(G-H) ..."
  , "  " ++ c_curve ++ "_Fp2_mont_sub_inplace(Ty,I);        // Y3 = theta*(G-H) - I"
  , "  " ++ c_curve ++ "_Fp2_mont_mul_inplace(Tz,E);        // Z3 = Z*E"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(theta ,Qx,A);         // theta*X2"
  , "  " ++ c_curve ++ "_Fp2_mont_mul(lambda,Qy,B);         // lambda*Y2"
  , "  " ++ c_curve ++ "_Fp2_mont_sub(A,B,J);               // J = theta*X2 - lambda*Y2"
  , ""
  , "  " ++ c_curve ++ "_Fp2_mont_scale_by_prime_field( Py, lambda, D );  // D = lambda * Y_p"
  , "  " ++ c_curve ++ "_Fp2_mont_scale_by_prime_field( Px, theta , C );  // C = theta  * X_p"
  , "  " ++ c_curve ++ "_Fp2_mont_neg_inplace(C);                         // ... - theta * X_p"
  , ""
  ] ++
  (case twist_type of
    DTwist -> ["  " ++ c_curve ++ "_pairing_combine_1_w_w3 (D,C,J,line);  // lambda*Y_p     - theta*X_p*w   + J*w^3"]
    MTwist -> ["  " ++ c_curve ++ "_pairing_combine_w3_w2_1(D,C,J,line);  // lambda*Y_p*w^3 - theta*X_p*w^2 + J"    ]
  ) ++
  [ "}"
  , ""
  , "//--------------------------------------"
  , ""
  , "// inputs:  projective coordinates of points P in G1 and Q in G2 (affine points!)"
  , "// outputs: the final value Fp12 and the final T point (projective, G2)"
  , "void " ++ c_curve ++ "_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f ) {"
  , "  uint64_t line[NWORDS_FP12];"
  , "  uint64_t f[NWORDS_FP12]; "
  , "  uint64_t T[3*NWORDS_FP2]; "
  , ""
  , "  " ++ c_curve ++ "_Fp12_mont_set_one(f);"
  , "  " ++ c_curve ++ "_G2_proj_from_affine(Q,T);"
  , ""
  , "  uint64_t x = " ++ c_curve ++ "_miller_loop_param;"
  , "  for(int i=MILLER_LOOP_LENGTH-1; i>=0; i--) {"
  , "    " ++ c_curve ++ "_Fp12_mont_sqr_inplace(f);"
  , "    " ++ c_curve ++ "_pairing_miller_double(P,T,line);"
  , "    " ++ c_curve ++ "_Fp12_mont_mul_inplace(f,line);"
  , "    if ((x>>i)&1) {"
  , "      " ++ c_curve ++ "_pairing_miller_mixed_add(P,Q,T,line);"
  , "      " ++ c_curve ++ "_Fp12_mont_mul_inplace(f,line);"
  , "    }"
  , "  }"
  , ""
  , "  " ++ c_curve ++ "_G2_proj_copy  (T, out_T);"
  , "  " ++ c_curve ++ "_Fp12_mont_copy(f, out_f);"
  , "}"
  , ""
  , "//------------------------------------------------------------------------------"
  , "// NOTE: currently the \"hard exponentiation\" dominates, so we should optimize that "
  , ""
  ] ++ 
  c_hard_expo params ++
  [ ""
  , "//--------------------------------------"
  , ""
  , ""
  , "// exponentiation (in Fp12) to the power `(p^12-1)/r`"
  , "void " ++ c_curve ++ "_pairing_final_expo(const uint64_t *src, uint64_t *tgt) {"
  , "  uint64_t A[NWORDS_FP12]; "
  , "  uint64_t B[NWORDS_FP12]; "
  , ""
  , "  " ++ c_curve ++ "_Fp12_mont_copy(src, A);"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^p"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^(p^2)"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^(p^3)"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^(p^4)"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^(p^5)"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(A);      // x^(p^6)"
  , "  " ++ c_curve ++ "_Fp12_mont_div_inplace(A, src);       // x^(p^6 - 1)"
  , ""
  , "  " ++ c_curve ++ "_Fp12_mont_copy(A, B);"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(B);      // y^p"
  , "  " ++ c_curve ++ "_Fp12_mont_frobenius_inplace(B);      // y^(p^2)"
  , "  " ++ c_curve ++ "_Fp12_mont_mul_inplace(B, A);         // y^(p^2 + 1)"
  , ""
  , "  " ++ c_curve ++ "_pairing_hard_expo(B, tgt);"
  , "}"
  , ""
  , "//------------------------------------------------------------------------------"
  , ""
  ] ++ 
  c_the_pairing params ++
  [ ""
  , "//------------------------------------------------------------------------------"
  , ""
  , "// P and Q are projective points in G1 and G2, respectively"
  , "// tgt is in Fp12"
  , "void " ++ c_curve ++ "_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {"
  , "  uint64_t aff_P[2*NWORDS_FP ]; "
  , "  uint64_t aff_Q[2*NWORDS_FP2]; "
  , ""
  , "  " ++ c_curve ++ "_G1_proj_to_affine( P, aff_P );"
  , "  " ++ c_curve ++ "_G2_proj_to_affine( Q, aff_Q );"
  , ""
  , "  " ++ c_curve ++ "_pairing_affine(aff_P, aff_Q, tgt);"
  , "}"
  , ""
  , "//------------------------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------

c_the_pairing :: PairingParams -> Code
c_the_pairing params@(PairingParams{..}) = case c_curve of
  "bn128"     -> c_bn128_pairing     params
  "bls12_381" -> c_bls12_381_pairing params

c_bn128_pairing :: PairingParams -> Code
c_bn128_pairing params@(PairingParams{..}) =
  [ "// computes the optimal Ate pairing for BN128. "
  , "// P and Q are affine points in G1 and G2, respectively"
  , "// tgt is in Fp12"
  , "void " ++ c_curve ++ "_pairing_affine(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {"
  , "  uint64_t f [NWORDS_FP12];      // Fp12"
  , "  uint64_t f2[NWORDS_FP12];      // Fp12"
  , "  uint64_t T [3*NWORDS_FP2];     // proj G2"
  , "  uint64_t T2[3*NWORDS_FP2];     // proj G2"
  , "  uint64_t phiQ [2*NWORDS_FP2];  // affine G2"
  , "  uint64_t phi2Q[2*NWORDS_FP2];  // affine G2"
  , ""
  , "  if ( " ++ c_curve ++ "_G1_affine_is_infinity(P) || " ++ c_curve ++ "_G2_affine_is_infinity(Q) ) {"
  , "    " ++ c_curve ++ "_Fp12_mont_set_one(tgt);"
  , "    return;"
  , "  }"
  , ""
  , "  " ++ c_curve ++ "_pairing_frobenius_G2(Q   , phiQ );          //  pi(Q)"
  , "  " ++ c_curve ++ "_pairing_frobenius_G2(phiQ, phi2Q);          //  pi^2(Q)"
  , "  " ++ c_curve ++ "_G2_affine_neg_inplace(phi2Q);               // -pi^2(Q)"
  , ""
  , "  " ++ c_curve ++ "_pairing_miller_loop(P,Q,T,f);"
  , ""
  , "  //uint64_t tmp[3*NWORDS_FP2];"
  , "  //bn128_G2_proj_from_affine(phiQ,tmp);"
  , "  //bn128_G2_proj_add(T,tmp,T2);"
  , "  " ++ c_curve ++ "_G2_proj_madd_proj_aff(T,phiQ,T2);           // T2 = T + phiQ;"
  , ""
  , "  " ++ c_curve ++ "_pairing_miller_mixed_add(P,phiQ,T,f2);      //         line(T, phiQ)"
  , "  " ++ c_curve ++ "_Fp12_mont_mul_inplace(f,f2);                // f = f * line(T, phiQ)"
  , ""
  , "  " ++ c_curve ++ "_pairing_miller_mixed_add(P,phi2Q,T2,f2);    //         line(T+phiQ, -phi2Q)"
  , "  " ++ c_curve ++ "_Fp12_mont_mul_inplace(f,f2);                // f = f * line(T+phiQ, -phi2Q)"
  , " "
  , "  " ++ c_curve ++ "_pairing_final_expo(f, tgt);"
  , "}"
  ]

c_bls12_381_pairing :: PairingParams -> Code
c_bls12_381_pairing params@(PairingParams{..}) =
  [ "// computes the optimal Ate pairing for BLS12-381. "
  , "// P and Q are affine points in G1 and G2, respectively"
  , "// tgt is in Fp12"
  , "void " ++ c_curve ++ "_pairing_affine(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {"
  , "  uint64_t f [NWORDS_FP12];      // Fp12"
  , "  uint64_t T [3*NWORDS_FP2];     // proj G2"
  , ""
  , "  if ( " ++ c_curve ++ "_G1_affine_is_infinity(P) || " ++ c_curve ++ "_G2_affine_is_infinity(Q) ) {"
  , "    " ++ c_curve ++ "_Fp12_mont_set_one(tgt);"
  , "    return;"
  , "  }"
  , ""
  , "  " ++ c_curve ++ "_pairing_miller_loop(P,Q,T,f);"
  , "  " ++ c_curve ++ "_pairing_final_expo(f,tgt);"
  , "}"
  ]

--------------------------------------------------------------------------------

c_header :: PairingParams -> Code
c_header params@(PairingParams{..}) = 
  [ "#include <stdint.h>"
  , ""
  , "void " ++ c_curve ++ "_pairing_affine    (const uint64_t *P, const uint64_t *Q, uint64_t *tgt);"
  , "void " ++ c_curve ++ "_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt);"
  , ""
  , "// for testing purposes:"
  , "void " ++ c_curve ++ "_pairing_psi        (const uint64_t *src, uint64_t *tgt);"
  , "void " ++ c_curve ++ "_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt);"
  , "void " ++ c_curve ++ "_pairing_final_expo (const uint64_t *src, uint64_t *tgt);"
  , "void " ++ c_curve ++ "_pairing_hard_expo  (const uint64_t *src, uint64_t *tgt);"
  , "void " ++ c_curve ++ "_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f );"
  ]

--------------------------------------------------------------------------------

hs_code :: PairingParams -> Code
hs_code params@(PairingParams{..}) = 
  [ "-- | Optimal Ate pairing for " ++ hs_curve ++ " curve"
  , ""
  , "-- NOTE 1: This module is intented to be imported qualified"
  , "-- NOTE 2: Generated code, do not edit!"
  , ""
  , "{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies #-}"
  , "module " ++ hsModule (mk_hs_path params) 
  , "  ( pairing"
  , "  , pairingProj"
  , "  )"
  , "  where"
  , ""
  , "--------------------------------------------------------------------------------"
  , "  "
  , "import Control.Monad"
  , "import System.IO.Unsafe"
  , ""
  , "import Data.Word"
  , "import Foreign.Ptr"
  , "import Foreign.ForeignPtr"
  , "import Foreign.Marshal.Alloc"
  , ""
  , "import ZK.Algebra.Class.Field   as F"
  , "import ZK.Algebra.Class.Curve   as C"
  , "import qualified ZK.Algebra.Class.Pairing as P"
  , ""
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Fr.Mont ( Fr   )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Poly    ( Poly )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Array   ()"
  , ""
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Fp.Mont   ( Fp   )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Fp2.Mont  ( Fp2  )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Fp6.Mont  ( Fp6  )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".Fp12.Mont ( Fp12 )"
  , ""
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".G1.Affine ( G1 )"
  , "import ZK.Algebra.Curves." ++ hs_curve ++ ".G2.Affine ( G2 )"
  , ""
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fr.Mont   as Fr"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fr.Std    as StdFr"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fp.Mont   as Fp"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fp2.Mont  as Fp2"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fp6.Mont  as Fp6"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".Fp12.Mont as Fp12"
  , ""
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".G1.Affine as AffG1"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".G2.Affine as AffG2"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".G1.Proj   as ProjG1"
  , "import qualified ZK.Algebra.Curves." ++ hs_curve ++ ".G2.Proj   as ProjG2"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "type ProjG1 = ProjG1.G1"
  , "type ProjG2 = ProjG2.G2"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "instance P.PairingCurve 'P." ++ hs_curve ++ " where"
  , "  type Fp     'P." ++ hs_curve ++ " = Fp.Fp"
  , "  type Fp2    'P." ++ hs_curve ++ " = Fp2.Fp2"
  , "  type Fp12   'P." ++ hs_curve ++ " = Fp12.Fp12"
  , "  type Fr     'P." ++ hs_curve ++ " = Fr.Fr"
  , "  type StdFr  'P." ++ hs_curve ++ " = StdFr.Fr"
  , "  type G1     'P." ++ hs_curve ++ " = AffG1.G1"
  , "  type G2     'P." ++ hs_curve ++ " = AffG2.G2"
  , "  type ProjG1 'P." ++ hs_curve ++ " = ProjG1.G1"
  , "  type ProjG2 'P." ++ hs_curve ++ " = ProjG2.G2"
  , "  type Poly   'P." ++ hs_curve ++ " = Poly"
  , ""
  , "  lowerSomeCurve _proxy = P." ++ hs_curve
  , "  pairing        _proxy = " ++ hsModule (mk_hs_path params) ++ ".pairing"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  , "-- void " ++ c_curve ++ "_pairing_affine    (const uint64_t *P, const uint64_t *Q, uint64_t *tgt);"
  , "-- void " ++ c_curve ++ "_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt);"
  , ""
  , "foreign import ccall unsafe \"" ++ c_curve ++ "_pairing_affine\"     c_pairing_affine     :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , "foreign import ccall unsafe \"" ++ c_curve ++ "_pairing_projective\" c_pairing_projective :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
  , ""
  , "{-# NOINLINE pairing #-}"
  , "pairing :: G1 -> G2 -> Fp12"
  , "pairing (AffG1.MkG1 fptr1) (AffG2.MkG2 fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (12*nwords_fp)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_pairing_affine ptr1 ptr2 ptr3"
  , "  return (Fp12.MkFp12 fptr3)"
  , ""
  , "{-# NOINLINE pairingProj #-}"
  , "pairingProj :: ProjG1 -> ProjG2 -> Fp12"
  , "pairingProj (ProjG1.MkG1 fptr1) (ProjG2.MkG2 fptr2) = unsafePerformIO $ do"
  , "  fptr3 <- mallocForeignPtrArray " ++ show (12*nwords_fp)
  , "  withForeignPtr fptr1 $ \\ptr1 -> do"
  , "    withForeignPtr fptr2 $ \\ptr2 -> do"
  , "      withForeignPtr fptr3 $ \\ptr3 -> do"
  , "        c_pairing_projective ptr1 ptr2 ptr3"
  , "  return (Fp12.MkFp12 fptr3)"
  , ""
  ]

--------------------------------------------------------------------------------

mk_c_path :: PairingParams -> Path
mk_c_path (PairingParams{..}) = Path ["curves","pairing",c_curve ++ "_pairing"]

mk_hs_path :: PairingParams -> Path
mk_hs_path (PairingParams{..}) = Path ["ZK","Algebra","Curves",hs_curve,"Pairing"]

curve_pairing_c_codegen :: FilePath -> PairingParams -> IO ()
curve_pairing_c_codegen tgtdir params@(PairingParams{..}) = do

  let fn_h = tgtdir </> (cFilePath "h" $ mk_c_path params)
  let fn_c = tgtdir </> (cFilePath "c" $ mk_c_path params)

  createTgtDirectory fn_h
  createTgtDirectory fn_c

  putStrLn $ "writing `" ++ fn_h ++ "`" 
  writeFile fn_h $ unlines $ c_header params

  putStrLn $ "writing `" ++ fn_c ++ "`" 
  writeFile fn_c $ unlines $ c_code  params

curve_pairing_hs_codegen :: FilePath -> PairingParams -> IO ()
curve_pairing_hs_codegen tgtdir params@(PairingParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath $ mk_hs_path params)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code params

--------------------------------------------------------------------------------
