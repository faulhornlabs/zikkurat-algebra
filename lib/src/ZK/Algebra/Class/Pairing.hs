
-- | Elliptic curve pairings

{-# LANGUAGE 
      ScopedTypeVariables, TypeFamilies, DataKinds, KindSignatures, TypeOperators,
      BangPatterns, StandaloneDeriving, FlexibleContexts
  #-}
module ZK.Algebra.Class.Pairing where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Kind

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve 
import ZK.Algebra.Class.Poly
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Vector
import ZK.Algebra.Class.FFT
import ZK.Algebra.Class.Misc

--------------------------------------------------------------------------------

-- | TODO: unify these from the different libraries
data SomeCurve
  = BN128    
  | BLS12_381
  deriving (Eq,Show)

--------------------------------------------------------------------------------

recognizeScalarField :: Integer -> Maybe SomeCurve
recognizeScalarField r = case r of
  21888242871839275222246405745257275088548364400416034343698204186575808495617 -> Just BN128
  0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001            -> Just BLS12_381
  _ -> Nothing

recognizeBaseField :: Integer -> Maybe SomeCurve
recognizeBaseField p = case p of
  21888242871839275222246405745257275088696311157297823662689037894645226208583                      -> Just BN128
  0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab -> Just BLS12_381
  _ -> Nothing

--------------------------------------------------------------------------------

-- | The class of pairing curves. 
--
-- For the lack of better ideas, the instances of this class are lifted types
-- corresponding to the constructors of 'SomeCurve'; all the relevant types are
-- associated type families.
--
-- In practice it seems you will also need to pass 'Proxy'-s to write code which
-- is generic over pairing curves...
--
-- (again, I wish for parametrized modules, importing instances, and stuff like that...)
--
class 
  ( PrimeField      (Fp   c)
  , QuadraticExt    (Fp2  c)
  , ExtField'       (Fp12 c)
  , FFTField        (Fr c)
  , PrimeField      (Fr c)
  , PrimeField      (StdFr c)
  , MontgomeryField (Fp c)
  , MontgomeryField (Fr c)
  , AffineCurve     (G1 c)
  , AffineCurve     (G2 c)
  , ProjCurve       (ProjG1 c)
  , ProjCurve       (ProjG2 c)
  , UnivariateFFT   (Poly c)
  , VectorSpace     (FlatArray (Fr c))
  , ExtBase         (Fp2  c) ~ Fp c
  , PrimeBase       (Fp12 c) ~ Fp c
  , StandardField   (Fr c) ~ StdFr c 
  , BaseField       (G1 c) ~ Fp  c
  , BaseField       (G2 c) ~ Fp2 c
  , ScalarField     (G1 c) ~ Fr  c
  , ScalarField     (G2 c) ~ Fr  c
  , AffinePoint     (ProjG1 c) ~ G1 c
  , AffinePoint     (ProjG2 c) ~ G2 c
  , Coeff           (Poly c) ~ Fr c
  ) => PairingCurve (c :: SomeCurve) 
  where
    type Fp     c :: Type
    type Fp2    c :: Type
    type Fp12   c :: Type
    type Fr     c :: Type
    type StdFr  c :: Type
    type G1     c :: Type
    type G2     c :: Type
    type ProjG1 c :: Type
    type ProjG2 c :: Type
    type Poly   c :: Type

    lowerSomeCurve :: Proxy c -> SomeCurve
    pairing        :: Proxy c -> G1 c -> G2 c -> Fp12 c

--------------------------------------------------------------------------------
