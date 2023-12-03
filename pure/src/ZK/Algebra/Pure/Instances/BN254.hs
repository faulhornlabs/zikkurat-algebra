
-- | The BN254 (aka alt-bn128) curve
--
-- See eg. <https://hackmd.io/@jpw/bn254>

{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module ZK.Algebra.Pure.Instances.BN254 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BN254 BaseField  )
type Fr = FF (CurveField BN254 ScalarField)

--------------------------------------------------------------------------------

type Fp2  = GF "u" 2 Fp
type Fp6  = GF "v" 3 Fp2
type Fp12 = GF "w" 2 Fp6
  
instance Ext "u" 2 Fp  where truncatedIrredPoly_ _ _ = [1,0]                            -- u^2 + 1
instance Ext "v" 3 Fp2 where truncatedIrredPoly_ _ _ = [xi,0,0] where xi = GF [-9,-1]   -- v^3 - (9+u)
instance Ext "w" 2 Fp6 where truncatedIrredPoly_ _ _ = [-1,0]                           -- w^2 - v

--------------------------------------------------------------------------------

a :: Fp
a = 123

b :: Fp2
b = GF [5,7]

c :: Fp6
c = GF [4,b,GF [1,3]]

d :: Fp12
d = GF [2,3]
