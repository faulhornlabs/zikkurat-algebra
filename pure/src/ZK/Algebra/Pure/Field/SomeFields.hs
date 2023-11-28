
{-# LANGUAGE 
      NumericUnderscores, DataKinds, KindSignatures, FlexibleInstances, 
      ScopedTypeVariables      
#-}
module ZK.Algebra.Pure.Field.SomeFields where

--------------------------------------------------------------------------------

import Data.Proxy
import GHC.TypeLits
import Numeric.Natural

import ZK.Algebra.Pure.Curve.SomeCurves
import ZK.Algebra.Pure.Misc

--------------------------------------------------------------------------------

data APrimeField
  = Goldilocks
  | Mersenne31
  | BabyBear
  | CurveField !ACurve !BaseOrScalar
  | GenericPrimeField !Natural -- Integer
  deriving (Eq,Show)

primeFieldName :: APrimeField -> String
primeFieldName = show

--------------------------------------------------------------------------------
-- type-level hackery

-- data CurveField' (c :: ACurve) (f :: BaseOrScalar) :: APrimeField = MkCurveField'
--  
-- data GenericPrimeField' (p :: Nat) :: APrimeField = MkGenericPrimeField'

class LowerPrimeField (t :: APrimeField) where
  loweredPrimeField :: Proxy t -> APrimeField

instance LowerPrimeField Goldilocks where loweredPrimeField _ = Goldilocks
instance LowerPrimeField Mersenne31 where loweredPrimeField _ = Mersenne31
instance LowerPrimeField BabyBear   where loweredPrimeField _ = BabyBear

instance LowerCurve c => LowerPrimeField ('CurveField c 'BaseField  ) where loweredPrimeField _ = CurveField (loweredCurve (Proxy :: Proxy c)) BaseField
instance LowerCurve c => LowerPrimeField ('CurveField c 'ScalarField) where loweredPrimeField _ = CurveField (loweredCurve (Proxy :: Proxy c)) ScalarField

instance KnownNat n => LowerPrimeField ('GenericPrimeField n) where loweredPrimeField _ = GenericPrimeField (integerToNatural $ natVal (Proxy :: Proxy n))

--------------------------------------------------------------------------------

fieldPrime :: APrimeField -> Integer
fieldPrime field = case field of
  Goldilocks             -> 0xffff_ffff_0000_0001       -- p = 2^64 - 2^32 + 1
  Mersenne31             -> 0x7fff_ffff                 -- p = 2^31 - 1
  BabyBear               -> 0x7800_0001                 -- p = 15*(2^27) + 1
  CurveField curve which -> curvePrimes curve which
  GenericPrimeField p    -> naturalToInteger p

--------------------------------------------------------------------------------
