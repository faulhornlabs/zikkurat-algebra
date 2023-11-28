
-- | Synonyms for the common field types

{-# LANGUAGE DataKinds #-}
module ZK.Algebra.Pure.Field.Synonyms.All where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type F_Goldilocks = FF Goldilocks
type F_Mersenne31 = FF Mersenne31
type F_BabyBear   = FF BabyBear

--------------------------------------------------------------------------------

type BN254_Fp = FF (CurveField BN254 BaseField  )
type BN254_Fr = FF (CurveField BN254 ScalarField)

--------------------------------------------------------------------------------

type BLS12_381_Fp = FF (CurveField BLS12_381 BaseField  )
type BLS12_381_Fr = FF (CurveField BLS12_381 ScalarField)

--------------------------------------------------------------------------------

type BLS12_377_Fp = FF (CurveField BLS12_377 BaseField  )
type BLS12_377_Fr = FF (CurveField BLS12_377 ScalarField)

--------------------------------------------------------------------------------

type Secp256k1_Fp = FF (CurveField Secp256k1 BaseField  )
type Secp256k1_Fr = FF (CurveField Secp256k1 ScalarField)

--------------------------------------------------------------------------------

type Curve25519_Fp = FF (CurveField Curve25519 BaseField  )
type Curve25519_Fr = FF (CurveField Curve25519 ScalarField)

--------------------------------------------------------------------------------
