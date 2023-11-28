
-- | Finite field implementations

{-# LANGUAGE KindSignatures, ScopedTypeVariables #-}
module ZK.Algebra.Pure.Field.Impl where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Ratio

import System.Random

import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Field.SomeFields
import ZK.Algebra.Pure.Curve.SomeCurves
import ZK.Algebra.Pure.Misc

import qualified ZK.Algebra.Pure.Field.Raw.Generic   as Gen
import qualified ZK.Algebra.Pure.Field.Raw.Extension as Ext

--------------------------------------------------------------------------------
-- * Prime fields, typed interface

-- | A prime field
newtype FF (t :: APrimeField) 
  = FF Integer
  deriving (Eq)

instance Show (FF t) where 
  show (FF x) = show x

extractPrime :: LowerPrimeField t => FF t -> Integer
extractPrime = fieldPrime . loweredPrimeField . ffToProxy

extractPrime1 :: LowerPrimeField t => f (FF t) -> Integer
extractPrime1= fieldPrime . loweredPrimeField . ffToProxy1 

instance LowerPrimeField t => Num (FF t) where
  fromInteger k = let y  = FF (mod k (extractPrime y)) in y
  negate x@(FF a)        = FF $ Gen.neg (extractPrime x) a
  (+)    x@(FF a) (FF b) = FF $ Gen.add (extractPrime x) a b
  (-)    x@(FF a) (FF b) = FF $ Gen.sub (extractPrime x) a b
  (*)    x@(FF a) (FF b) = FF $ Gen.mul (extractPrime x) a b
  abs    = error "FF/abs"
  signum = error "FF/signum"

instance LowerPrimeField t => Fractional (FF t) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip  x@(FF a)        = FF $ Gen.inv (extractPrime x) a
  (/)    x@(FF a) (FF b) = FF $ Gen.div (extractPrime x) a b

ffPower :: LowerPrimeField t => FF t -> Integer -> FF t
ffPower x@(FF a) e = FF $ Gen.pow (extractPrime x) a e 

instance LowerPrimeField t => Rnd (FF t) where
  rndIO = action where
    action = FF <$> randomRIO (0,prime-1) 
    prime  = extractPrime1 action

instance LowerPrimeField t => Serialize (FF t) where
  sizeInWords proxy = integerRequiredNumberOfWords (extractPrime1 proxy)
  toWords f@(FF n)  = integerToWordsLE (sizeInWords $ ffToProxyFF f) n 
  fromWords ws      = fromInteger (integerFromWordsLE ws)

instance LowerPrimeField t => Field (FF t) where
  fieldName      pxy = primeFieldName (loweredPrimeField (ffToProxy1 pxy))
  characteristic pxy = extractPrime1 pxy
  dimension      pxy = 1
  fieldSize      pxy = extractPrime1 pxy
  primGenPxy     pxy = error "FF/primGen: not implemented yet"
  pow           = ffPower
  isZero (FF a) = (a == 0) 
  isOne  (FF a) = (a == 1)
  frobenius     = id
  projectToPrimeSubfield (FF a) = Just a

instance LowerPrimeField t => PrimeField (FF t) where
  asInteger (FF a) = a

--------------------------------------------------------------------------------

ffToProxy :: FF t -> Proxy t
ffToProxy _ = Proxy

ffToProxy1 :: f (FF t) -> Proxy t
ffToProxy1 _ = Proxy

ffToProxyFF :: FF t -> Proxy (FF t)
ffToProxyFF _ = Proxy

--------------------------------------------------------------------------------
