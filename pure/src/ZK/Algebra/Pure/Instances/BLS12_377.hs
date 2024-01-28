
-- | The BLS12-377 curve
--
-- See eg. <https://pkg.go.dev/github.com/consensys/gnark-crypto/ecc/bls12-377>

{-# LANGUAGE DataKinds, MultiParamTypeClasses,FlexibleInstances, TypeSynonymInstances #-}
module ZK.Algebra.Pure.Instances.BLS12_377 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BLS12_377 BaseField  )
type Fr = FF (CurveField BLS12_377 ScalarField)

--------------------------------------------------------------------------------

type Fp2  = GF "u" 2 Fp
type Fp6  = GF "v" 3 Fp2
type Fp12 = GF "w" 2 Fp6
  
instance Ext "u" 2 Fp  where truncatedIrredPoly_ _ _ = [5,0]                               -- u^2 + 5
instance Ext "v" 3 Fp2 where truncatedIrredPoly_ _ _ = [mu,0,0] where mu = GF [0,-1]       -- v^3 - u
instance Ext "w" 2 Fp6 where truncatedIrredPoly_ _ _ = [mv,0]   where mv = GF [0,-1,0]     -- w^2 - v

--------------------------------------------------------------------------------
