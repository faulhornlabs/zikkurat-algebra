
-- | The BN254 (aka alt-bn128) curve

{-# LANGUAGE DataKinds #-}
module ZK.Algebra.Pure.Field.Synonyms.BN254 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BN254 BaseField  )
type Fr = FF (CurveField BN254 ScalarField)

--------------------------------------------------------------------------------

