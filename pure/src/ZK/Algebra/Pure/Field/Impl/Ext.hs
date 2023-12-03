
-- | Finite field extension implementations

{-# LANGUAGE 
      MultiParamTypeClasses, TypeFamilies, DataKinds, KindSignatures, 
      ScopedTypeVariables, TypeApplications #-}

module ZK.Algebra.Pure.Field.Impl.Ext where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Ratio
import Data.List
import Data.Maybe

import GHC.TypeLits
import Data.Kind

import Control.Monad
import System.Random

import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Field.SomeFields
import ZK.Algebra.Pure.Curve.SomeCurves
import ZK.Algebra.Pure.Misc

import qualified ZK.Algebra.Pure.Field.Raw.Extension as E

--------------------------------------------------------------------------------
-- * Extension fields, typed interface

-- | An extension field. The idea (in a lack of a better one) is the variable, 
-- degree and base type determines the defining irreducible polynomial in practice
-- (you have to make it an instance of @Ext@)
newtype GF (var :: Symbol) (deg :: Nat) (base :: Type) 
  = GF [base]

instance (Eq base, Num base) => Eq (GF var deg base) where
  (==) (GF xs) (GF ys) = E.extEq xs ys

constantCoeff :: Num base => GF var deg base -> base
constantCoeff (GF xs) = case xs of 
  []    -> 0 
  (x:_) -> x 

--------------------------------------------------------------------------------

showGF :: (Field base, Ext var deg base) => GF var deg base -> String
showGF input@(GF coeffs) = "(" ++ showPoly (gfVarString input) coeffs ++ ")"

showPoly :: forall t. Field t => String -> [t] -> String
showPoly var coeffs = 
  case catMaybes (zipWith f [0..] coeffs) of
    [] -> "0"
    xs  -> intercalate " + " (reverse xs)

  where
    f :: Int -> t -> Maybe String
    f k c = if isZero c then Nothing else Just (g k c)

    g :: Int -> t -> String
    g 0 c = show c
    g 1 c = if isOne c then var                  else show c ++ "*" ++ var 
    g k c = if isOne c then var ++ "^" ++ show k else show c ++ "*" ++ var ++ "^" ++ show k

--------------------------------------------------------------------------------
-- * Defining polynomial

class (KnownSymbol var, KnownNat deg, Field base) => Ext (var :: Symbol) (deg :: Nat) (base :: Type) where
  truncatedIrredPoly_ :: Proxy var -> Proxy deg -> [base]

irredPoly_ :: (Num base, Ext var deg base) => Proxy var -> Proxy deg -> [base]
irredPoly_ pxy1 pxy2 = truncatedIrredPoly_ pxy1 pxy2 ++ [1]

extractIrredPoly :: Ext var deg base => GF var deg base -> [base]
extractIrredPoly gf = irredPoly_ (gfVarProxy gf) (gfDegProxy gf)

extractIrredPoly1 :: Ext var deg base => f (GF var deg base) -> [base]
extractIrredPoly1 xx = irredPoly_ (gfVarProxy1 xx) (gfDegProxy1 xx)

showIrredPoly :: Ext var deg base => GF var deg base -> String
showIrredPoly gf = showPoly (gfVarString gf) (extractIrredPoly gf)

showIrredPoly1 :: Ext var deg base => f (GF var deg base) -> String
showIrredPoly1 xx = showPoly (gfVarString1 xx) (extractIrredPoly1 xx)

--------------------------------------------------------------------------------
-- * instances

instance (Field base, Ext var deg base) => Show (GF var deg base) where 
  show = showGF

instance (Field base, Ext var deg base) => Num (GF var deg base) where
  fromInteger k = GF [fromInteger k]
  negate x@(GF as)         = GF $ E.extNeg as
  (+)    x@(GF as) (GF bs) = GF $ E.extAdd as bs
  (-)    x@(GF as) (GF bs) = GF $ E.extSub as bs
  (*)    x@(GF as) (GF bs) = GF $ E.extMul (extractIrredPoly x) as bs
  abs    = error "GF/abs"
  signum = error "GF/signum"

instance (Field base, Ext var deg base) => Fractional (GF var deg base) where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip  x@(GF as)         = GF $ E.extInv (extractIrredPoly x) as
  (/)    x@(GF as) (GF bs) = GF $ E.extDiv (extractIrredPoly x) as bs

gfPower :: (Field base, Ext var deg base) => GF var deg base -> Integer -> GF var deg base
gfPower x@(GF as) e = GF $ E.extPow (extractIrredPoly x) as e 

instance (Rnd base, Ext var deg base) => Rnd (GF var deg base) where
  rndIO = action where
    action = GF <$> replicateM degree rndIO
    degree = extractDegree1 action

instance (Serialize base, Ext var deg base) => Serialize (GF var deg base) where
  sizeInWords pxy = (sizeInWords $ gfBaseProxy1 pxy)  * (extractDegree1 pxy)
  toWords (GF xs) = concatMap toWords xs

  parseWords ws = gfRest where 

    gfRest = case mb of 
      Just (cs, rest) -> Just (GF cs, rest)
      Nothing         -> Nothing

    mb  = go deg ws 
    deg = extractDegree1 (mbFstProxy gfRest)

    go !0 ws = Just ([],ws)
    go !k ws = case parseWords ws of
      Nothing       -> Nothing
      Just (x,rest) -> constFst x <$> go (k-1) rest

    constFst :: a -> ([a],b) -> ([a],b)
    constFst x (xs,y) = (x:xs, y)

    mbFstProxy :: Maybe (a,b) -> Proxy a
    mbFstProxy _ = Proxy

instance (Field base, Ext var deg base) => Field (GF var deg base) where
  fieldName      pxy = fieldName (gfBaseProxy1 pxy) ++ "[" ++ gfVarString1 pxy ++ "]/(" ++ showIrredPoly1 pxy ++ ")"
  characteristic pxy = (characteristic $ gfBaseProxy1 pxy)
  dimension      pxy = (extractDegree1 pxy) * (dimension $ gfBaseProxy1 pxy)
  fieldSize      pxy = (fieldSize $ gfBaseProxy1 pxy) ^ (extractDegree1 pxy)
  primGenPxy     pxy = error "GF/primGen: not implemented yet"
  isZero (GF as)     = all isZero as
  isOne  (GF as)     = case as of { [] -> False ; (x:xs) -> isOne x && all isZero xs }
  frobenius          = error "GF/frobenius"
  pow                = gfPower
  projectToPrimeSubfield = projectToPrimeSubfield . constantCoeff

instance (Field base, Ext var deg base) => ExtField (GF var deg base) where
  type ExtBase (GF var deg base) = base
  extDeg pxy       = extractDegree1 pxy
  embedExtBase x   = GF [x]
  projectToExtBase (GF xs) = case xs of
    []  -> Just 0
    [x] -> Just x
    _   -> Nothing

--------------------------------------------------------------------------------
-- * type level hackery

gfVarProxy :: GF var deg base -> Proxy var
gfVarProxy _ = Proxy

gfVarProxy1 :: f (GF var deg base) -> Proxy var
gfVarProxy1 _ = Proxy

gfVarString :: KnownSymbol var => GF var deg base -> String
gfVarString = symbolVal . gfVarProxy

gfVarString1 :: KnownSymbol var => f (GF var deg base) -> String
gfVarString1 = symbolVal . gfVarProxy1

--------------------

gfDegProxy :: GF var deg base -> Proxy deg
gfDegProxy _ = Proxy

gfDegProxy1 :: f (GF var deg base) -> Proxy deg
gfDegProxy1 _ = Proxy

gfDegree :: KnownNat deg => GF var deg base -> Int
gfDegree = fromInteger . natVal . gfDegProxy

--------------------

extractDegree :: KnownNat deg => GF var deg base -> Int
extractDegree = gfDegree 

extractDegree1 :: KnownNat deg => f (GF var deg base) -> Int
extractDegree1 = fromInteger . natVal . gfDegProxy1

--------------------

gfBaseProxy :: GF var deg base -> Proxy base
gfBaseProxy _ = Proxy

gfBaseProxy1 :: f (GF var deg base) -> Proxy base
gfBaseProxy1 _ = Proxy

--------------------------------------------------------------------------------
