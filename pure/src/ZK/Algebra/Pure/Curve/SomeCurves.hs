
-- TODO: factor out the curve database into its own package, 
-- and use that everywhere? (for example also in zikkurat-formats)

{-# LANGUAGE NumericUnderscores, DataKinds, KindSignatures #-}
module ZK.Algebra.Pure.Curve.SomeCurves where

--------------------------------------------------------------------------------

import Data.Proxy

--------------------------------------------------------------------------------

-- | Standard elliptic curves
data ACurve
  = BN254     | BabyJubJub | Bandersnatch | Grumpkin 
  | BLS12_381 | JubJub
  | BLS12_377 
  | Pallas    | Vesta
  | Secp256k1 | Secq256k1
  | Curve25519
  deriving (Eq,Show)

curveName :: ACurve -> String
curveName = show

--------------------------------------------------------------------------------

-- | Usually we have two prime fields associated to an elliptic curve
data BaseOrScalar
  = BaseField
  | ScalarField
  deriving (Eq,Show)

switchBaseScalar :: BaseOrScalar -> BaseOrScalar
switchBaseScalar which = case which of
  BaseField   -> ScalarField
  ScalarField -> BaseField

--------------------------------------------------------------------------------
-- type-level hackery

class LowerCurve (c :: ACurve) where
  loweredCurve :: Proxy c -> ACurve

instance LowerCurve BN254        where loweredCurve _ = BN254
instance LowerCurve BLS12_381    where loweredCurve _ = BLS12_381
instance LowerCurve BLS12_377    where loweredCurve _ = BLS12_377
instance LowerCurve Pallas       where loweredCurve _ = Pallas
instance LowerCurve Vesta        where loweredCurve _ = Vesta
instance LowerCurve JubJub       where loweredCurve _ = JubJub
instance LowerCurve BabyJubJub   where loweredCurve _ = BabyJubJub
instance LowerCurve Bandersnatch where loweredCurve _ = Bandersnatch       
instance LowerCurve Grumpkin     where loweredCurve _ = Grumpkin       
instance LowerCurve Secp256k1    where loweredCurve _ = Secp256k1       
instance LowerCurve Secq256k1    where loweredCurve _ = Secq256k1      
instance LowerCurve Curve25519   where loweredCurve _ = Curve25519      

--------------------------------------------------------------------------------

curvePrimes :: ACurve -> BaseOrScalar -> Integer
curvePrimes curve which = case curve of

  BN254 -> case which of 
    BaseField    -> 21888242871839275222246405745257275088696311157297823662689037894645226208583 
    ScalarField  -> 21888242871839275222246405745257275088548364400416034343698204186575808495617

  BLS12_381 -> case which of
    BaseField    -> 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab   
    ScalarField  -> 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001     

  BLS12_377 -> case which of
    BaseField    -> 0x01ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000001
    ScalarField  -> 0x12ab655e9a2ca55660b44d1e5c37b00159aa76fed00000010a11800000000001

  JubJub -> case which of
    BaseField    -> 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    ScalarField  -> 0xe7db4ea6533afa906673b0101343b00a6682093ccc81082d0970e5ed6f72cb7              -- cofactor = 8

  Bandersnatch -> case which of
    BaseField    -> 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
    ScalarField  -> 0x1cfb69d4ca675f520cce760202687600ff8f87007419047174fd06b52876e7e1             -- cofactor = 4

  BabyJubJub -> case which of
    BaseField    -> 21888242871839275222246405745257275088548364400416034343698204186575808495617
    ScalarField  -> 2736030358979909402780800718157159386076813972158567259200215660948447373041   -- cofactor = 8

  Pallas -> case which of
    BaseField   -> 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001
    ScalarField -> 0x40000000000000000000000000000000224698fc0994a8dd8c46eb2100000001

  Secp256k1 -> case which of
    BaseField    -> 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_FFFFFC2F
    ScalarField  -> 0xFFFFFFFF_FFFFFFFF_FFFFFFFF_FFFFFFFE_BAAEDCE6_AF48A03B_BFD25E8C_D0364141

  Curve25519 -> case which of
    BaseField    -> 57896044618658097711785492504343953926634992332820282019728792003956564819949  -- p = 2^255 - 19
    ScalarField  -> 7237005577332262213973186563042994240857116359379907606001950938285454250989   -- cofactor = 8

  Vesta     -> curvePrimes Pallas    (switchBaseScalar which)
  Grumpkin  -> curvePrimes BN254     (switchBaseScalar which)
  Secq256k1 -> curvePrimes Secp256k1 (switchBaseScalar which)

--------------------------------------------------------------------------------

