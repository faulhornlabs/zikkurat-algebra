
--
-- TODO: projective coordinates
--

{-# LANGUAGE 
      BangPatterns, StrictData, ScopedTypeVariables, TypeFamilies, 
      TypeApplications, FlexibleContexts, FunctionalDependencies,
      TypeOperators
  #-}

module ZK.Algebra.Pure.Curve.Class where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Kind
import Data.Maybe

import System.Random (Random)

import ZK.Algebra.Pure.Group
import ZK.Algebra.Pure.Field


--------------------------------------------------------------------------------
-- * Curve equations

-- | The short Weierstrass equation 
--
-- > y^2 = x^3 + A*x + B
--
data WeierstrassEq t = MkWeierstrassEq
  { _weierstrassA :: !t
  , _weierstrassB :: !t
  }
  deriving (Eq,Show)

-- | The Edwards curve equation 
--
-- > x^2 + y^2 = 1 + D*x^2*y^2
--
data EdwardsEq t = MkEdwardsEq
  { _edwardsD :: !t
  }
  deriving (Eq,Show)

-- | The twisted Edwards curve equation 
--
-- > A*x^2 + y^2 = 1 + D*x^2*y^2
--
data TwistedEdwardsEq t = MkTwistedEdwardsEq
  { _twistedEdwardsA :: !t
  , _twistedEdwardsD :: !t
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------
-- * Elliptic curves

class (Field (BaseField c), Eq c, Group c) => Curve c where
  -- | the field @Fq@ over which the curve is defined
  type BaseField c :: Type                       
  -- | r = size of the /cyclic/ subgroup of the curve we will use
  curveSubgroupOrder :: Proxy c -> Integer    
  -- | the cofactor is the number @c = |E(Fq)| / r@ 
  cofactor   :: Proxy c -> Integer            
  -- | the generator of the cyclic subgroup of size @r@
  basePoint  :: c                             

-- | number of points in the curve
curveOrder :: Curve c => Proxy c -> Integer
curveOrder proxy = grpOrder proxy

curveSubgroup :: forall c. Curve c => Subgroup c
curveSubgroup = Subgroup basePoint (fromInteger $ curveSubgroupOrder $ Proxy @c)

--------------------------------------------------------------------------------
-- * Elliptic curves in short Weierstrass form

class (Curve c, Show c, Show (BaseField c)) => WeierstrassCurve c where
  -- | the curve equation
  curveEq    :: Proxy c -> WeierstrassEq (BaseField c)           
  -- | the curve point at infinity
  pointAtInf :: c                                          
  -- | constructing (curve) points
  mkPoint    :: BaseField c -> BaseField c -> c            
  -- | deconstructing curve points (@Nothing@ = point at infinity)
  mbCoords   :: c -> Maybe (BaseField c, BaseField c)      
  -- | check whether a point is really on the curve
  isOnCurve  :: c -> Bool                                  

isECInf :: WeierstrassCurve c => c -> Bool
isECInf  = isNothing . mbCoords

xcoord, ycoord  :: WeierstrassCurve c => c -> BaseField c
xcoord c = case mbCoords c of { Just (x,_) -> x ; Nothing -> error "WeierstrassCurve/xcoord: point at infinity" }
ycoord c = case mbCoords c of { Just (_,y) -> y ; Nothing -> error "WeierstrassCurve/ycoord: point at infinity" }

toProjCoords :: WeierstrassCurve c => c -> (BaseField c, BaseField c, BaseField c)
toProjCoords c = case mbCoords c of
  Just (x,y) -> (x,y,1)
  Nothing    -> (0,1,0) 

--------------------------------------------------------------------------------

data Slope f 
  = FiniteSlope !f
  | InfiniteSlope
  deriving (Eq,Show)

-- | The slope of the line connecting two points P and Q.
slope :: forall c. WeierstrassCurve c => c -> c -> Slope (BaseField c)
slope p1 p2 = case p1 == p2 of

  False -> case mbCoords p1 of
    Nothing      -> InfiniteSlope
    Just (x1,y1) -> case mbCoords p2 of
      Nothing      -> InfiniteSlope
      Just (x2,y2) -> if x1 == x2
        then InfiniteSlope
        else FiniteSlope $ (y2 - y1) / (x2 - x1)

  True  -> case mbCoords p1 of 
    Nothing    -> InfiniteSlope
    Just (x,y) -> case curveEq (Proxy @c) of
      MkWeierstrassEq a _ -> if y == 0
        then InfiniteSlope
        else FiniteSlope $ (3*x*x + a) / (y + y)  

--------------------------------------------------------------------------------
-- * Pairing-friendly elliptic curves

class (Curve c, Curve e, e ~ ExtCurve c, Field (ExtBaseField c), PrimeField (ScalarField c), FFTField (ScalarField c), Random (ScalarField c)) 
      => PairingCurve c e | c -> e , e-> c where
  -- | the field @Fr@ corresponding to the useful (=large, cyclic) subgroup of size r
  type ScalarField  c :: Type           
  -- | the extension field @F_{q^k}@ where @k@ is the embedding degree
  type ExtBaseField c :: Type
  -- | the same curve but over the extension field
  type ExtCurve     c :: Type           
  -- | embed the curve over the base field into the curve over the extension field
  embedExtCurve :: c -> e
  -- | compute the pairing
  pairing       :: c -> e -> ExtBaseField c

basePointG1 :: forall c e. PairingCurve c e => c
basePointG1 = basePoint @c

basePointG2 :: forall c e. PairingCurve c e => e
basePointG2 = basePoint @e

type G1 c = c
type G2 c = ExtCurve c
type Gt c = ExtBaseField c

scaleG1 :: PairingCurve c e => ScalarField c -> c -> c  -- G1 c -> G1 c
scaleG1 scalar g1 = grpScale (asInteger scalar) g1

scaleG2 :: PairingCurve c e => ScalarField c -> e -> e  -- G2 c -> G2 c
scaleG2 scalar g2 = grpScale (asInteger scalar) g2

--------------------------------------------------------------------------------