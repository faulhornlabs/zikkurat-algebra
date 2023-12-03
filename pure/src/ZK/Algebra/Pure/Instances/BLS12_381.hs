
-- | The BLS12-381 curve
--
-- See eg.:
--
-- * <https://hackmd.io/@benjaminion/bls12-381>
--
-- * <https://github.com/nccgroup/pairing-bls12381/blob/master/Crypto/Pairing_bls12381.hs>
--

{-# LANGUAGE DataKinds, MultiParamTypeClasses,FlexibleInstances, TypeSynonymInstances #-}
module ZK.Algebra.Pure.Instances.BLS12_381 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BLS12_381 BaseField  )
type Fr = FF (CurveField BLS12_381 ScalarField)

--------------------------------------------------------------------------------

type Fp2  = GF "u" 2 Fp
type Fp6  = GF "v" 3 Fp2
type Fp12 = GF "w" 2 Fp6
  
instance Ext "u" 2 Fp  where truncatedIrredPoly_ _ _ = [1,0]                            -- u^2 + 1
instance Ext "v" 3 Fp2 where truncatedIrredPoly_ _ _ = [xi,0,0] where xi = GF [-1,-1]   -- v^3 - (1+u)
instance Ext "w" 2 Fp6 where truncatedIrredPoly_ _ _ = [-1,0]                           -- w^2 - v

--------------------------------------------------------------------------------
