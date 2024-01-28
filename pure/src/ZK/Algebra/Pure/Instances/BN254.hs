
-- | The BN254 (aka alt-bn128) curve
--
-- See eg. <https://hackmd.io/@jpw/bn254>

{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module ZK.Algebra.Pure.Instances.BN254 where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Impl
import ZK.Algebra.Pure.Field.Class

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BN254 BaseField  )
type Fr = FF (CurveField BN254 ScalarField)

--------------------------------------------------------------------------------

type Fp2  = GF "u" 2 Fp
type Fp6  = GF "v" 3 Fp2
type Fp12 = GF "w" 2 Fp6
  
instance Ext "u" 2 Fp  where truncatedIrredPoly_ _ _ = [1,0]                              -- u^2 + 1
instance Ext "v" 3 Fp2 where truncatedIrredPoly_ _ _ = [xi,0,0] where xi = GF [-9,-1]     -- v^3 - (9+u)
instance Ext "w" 2 Fp6 where truncatedIrredPoly_ _ _ = [mv,0]   where mv = GF [ 0,-1,0]   -- w^2 - v

--------------------------------------------------------------------------------

sanityCheckBN254 :: Bool
sanityCheckBN254 = and 
  [ sanityCheckFp2
  , sanityCheckFp6
  , sanityCheckFp12
  ]

sanityCheckFp2 :: Bool
sanityCheckFp2 = (ize2*ize2 == packPrimeBase reference2) where
  list2 = [ fromInteger (111*i) | i<-[1..2] ] :: [Fp]
  ize2  = packPrimeBase list2 :: Fp2
  reference2 :: [Fp]
  reference2 = 
    [ 21888242871839275222246405745257275088696311157297823662689037894645226171620
    , 49284
    ]

sanityCheckFp6 :: Bool
sanityCheckFp6 = (ize6*ize6 == packPrimeBase reference6) where
  list6 = [ fromInteger (111*i) | i<-[1..6] ] :: [Fp]
  ize6  = packPrimeBase list6 :: Fp6
  reference6 :: [Fp]
  reference6 = 
    [ 21888242871839275222246405745257275088696311157297823662689037894645223239222
    , 8255070
    , 21888242871839275222246405745257275088696311157297823662689037894645224126334
    , 6764229
    , 21888242871839275222246405745257275088696311157297823662689037894645225949842
    , 689976
    ]

sanityCheckFp12 :: Bool
sanityCheckFp12 = (ize12*ize12 == packPrimeBase reference12) where
  list12 = [ fromInteger (111*i) | i<-[1..12] ] :: [Fp]
  ize12  = packPrimeBase list12 :: Fp12
  reference12 :: [Fp]
  reference12 =
    [ 21888242871839275222246405745257275088696311157297823662689037894645210462345
    , 65658609
    , 21888242871839275222246405745257275088696311157297823662689037894645213912225
    , 55974303
    , 21888242871839275222246405745257275088696311157297823662689037894645219727737
    , 33180453
    , 21888242871839275222246405745257275088696311157297823662689037894645214799337
    , 40610016
    , 21888242871839275222246405745257275088696311157297823662689037894645218791341
    , 29496474
    , 21888242871839275222246405745257275088696311157297823662689037894645225247545
    , 4484844
    ]

{- 

tested against Sage
-------------------

p  = 21888242871839275222246405745257275088696311157297823662689037894645226208583
Fp = GF(p)

R.<x>   = Fp[]
Fp2.<u> = Fp.extension(x^2+1)
 
R.<x>    = Fp2[]
Fp12.<w> = Fp2.extension(x^6 - (9+u))

ize2 = Fp2(111+222*u)
print(ize2^2)

v = w^2
ize6 = Fp12( (111+222*u) + v*(333+444*u) + v^2*(555+666*u) )
print(ize6^2)

v = w^2;
ize12 = (111+222*u) + v*(333+444*u) + v^2*(555+666*u) + w*( (777+888*u) + v*(999+1110*u) + v^2*(1221+1332*u) )
print(ize12)
print(ize12^2)

-}

--------------------------------------------------------------------------------
