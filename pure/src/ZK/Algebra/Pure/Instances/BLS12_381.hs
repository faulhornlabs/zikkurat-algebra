
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
import ZK.Algebra.Pure.Field.Class

import ZK.Algebra.Pure.Field.SomeFields as F
import ZK.Algebra.Pure.Curve.SomeCurves as C

--------------------------------------------------------------------------------

type Fp = FF (CurveField BLS12_381 BaseField  )
type Fr = FF (CurveField BLS12_381 ScalarField)

--------------------------------------------------------------------------------

type Fp2  = GF "u" 2 Fp
type Fp6  = GF "v" 3 Fp2
type Fp12 = GF "w" 2 Fp6
  
instance Ext "u" 2 Fp  where truncatedIrredPoly_ _ _ = [1,0]                             -- u^2 + 1
instance Ext "v" 3 Fp2 where truncatedIrredPoly_ _ _ = [xi,0,0] where xi = GF [-1,-1]    -- v^3 - (1+u)
instance Ext "w" 2 Fp6 where truncatedIrredPoly_ _ _ = [mv,0]   where mv = GF [ 0,-1,0]  -- w^2 - v

--------------------------------------------------------------------------------

sanityCheckBLS12_381 :: Bool
sanityCheckBLS12_381 = and 
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
    [ 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272522824
    , 49284
    ] :: [Fp]

sanityCheckFp6 :: Bool
sanityCheckFp6 = (ize6*ize6 == packPrimeBase reference6) where
  list6 = [ fromInteger (111*i) | i<-[1..6] ] :: [Fp]
  ize6  = packPrimeBase list6 :: Fp6
  reference6 :: [Fp]
  reference6 = 
    [ 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894271364650
    , 763902
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894271561786
    , 850149
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272301046
    , 689976
    ] :: [Fp]

sanityCheckFp12 :: Bool
sanityCheckFp12 = (ize12*ize12 == packPrimeBase reference12) where
  list12 = [ fromInteger (111*i) | i<-[1..12] ] :: [Fp]
  ize12  = packPrimeBase list12 :: Fp12
  reference12 :: [Fp]
  reference12 = 
    [ 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894264206149
    , 6517809
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894265487533
    , 7084575
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894268346005
    , 7158501
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894267064621
    , 4336992
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894268493857
    , 4657338
    , 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894271598749
    , 4484844
    ] :: [Fp]

--------------------------------------------------------------------------------

{- 

tested against Sage
-------------------

p  = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
Fp = GF(p)

R.<x>   = Fp[]
Fp2.<u> = Fp.extension(x^2+1)

R.<x>    = Fp2[]
Fp12.<w> = Fp2.extension(x^6 - (1+u))

ize2 = Fp2(111+222*u)
print(ize2^2)

v = w^2;
ize6 = Fp12( (111+222*u) + v*(333+444*u) + v^2*(555+666*u) )
print(ize6^2)

v = w^2;
ize12 = (111+222*u) + v*(333+444*u) + v^2*(555+666*u) + w*( (777+888*u) + v*(999+1110*u) + v^2*(1221+1332*u) )
print(ize12)
print(ize12^2)

-}

--------------------------------------------------------------------------------


