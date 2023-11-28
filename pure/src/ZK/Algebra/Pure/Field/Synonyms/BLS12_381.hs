
-- | The BLS12-381 curve

{-# LANGUAGE DataKinds #-}
module ZK.Algebra.Pure.Field.Synonyms.BLS12_381 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BLS12_381 BaseField  )
type Fr = FF (CurveField BLS12_381 ScalarField)

--------------------------------------------------------------------------------
