
-- | The Tom256 (aka alt-bn128) curve
--
-- See eg. <https://hackmd.io/@jpw/bn254>

{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module ZK.Algebra.Pure.Instances.Tom256 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl
import ZK.Algebra.Pure.Field.Class

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField Tom256 BaseField  )
type Fr = FF (CurveField Tom256 ScalarField)

--------------------------------------------------------------------------------

