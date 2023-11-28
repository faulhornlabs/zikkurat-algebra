
-- | The BLS12-377 curve

{-# LANGUAGE DataKinds #-}
module ZK.Algebra.Pure.Field.Synonyms.BLS12_377 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BLS12_377 BaseField  )
type Fr = FF (CurveField BLS12_377 ScalarField)

--------------------------------------------------------------------------------
