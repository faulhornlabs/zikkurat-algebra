
{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module ZK.Algebra.Class.Curve where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy

import ZK.Algebra.Class.Field

--------------------------------------------------------------------------------

-- | Equality of the underlying representation
class StrictEq a where
  (===) :: a -> a -> Bool

--------------------------------------------------------------------------------
-- * Finite groups

-- | The class of (finite) groups
class (Eq a, StrictEq a, Show a, Rnd a) => Group a where
  -- | name of the group
  grpName :: Proxy a -> String
  -- | checks whether an element is the unit of the group
  grpIsUnit    :: a -> Bool
  -- | normalizes the underlying representation (for example when using projective coordinates)
  grpNormalize :: a -> a
  -- | the unit element of the group
  grpUnit      :: a
  -- | negation of group elements
  grpNeg       :: a -> a
  -- | doubling of group elements
  grpDbl       :: a -> a
  -- | addition of group elements
  grpAdd       :: a -> a -> a
  -- | subtraction of group elements
  grpSub       :: a -> a -> a
  -- | scaling of group elements
  grpScale     :: Integer -> a -> a
  grpScale_    :: Int     -> a -> a

  -- grpSize :: Proxy a -> Integer

--------------------------------------------------------------------------------
-- * Elliptic curves

class (Group a, Field (BaseField a), Field (ScalarField a)) => Curve a where
  -- | name of the ring
  curveNamePxy :: Proxy a -> String            
  -- | base field
  type BaseField a :: *
  -- | scalar field
  type ScalarField a :: *
  -- | check whether a point is on the curve
  isOnCurve   :: a -> Bool
  -- | check for being the point at infinity
  isInifinity :: a -> Bool
  -- | the infinity element
  infinity    :: a
  -- | the subgroup generator
  subgroupGen :: a
  -- | scalar multiplication
  scalarMul   :: ScalarField a -> a -> a

--------------------------------------------------------------------------------
-- * Affine curves

class Curve a => AffineCurve a where
  -- | affine coordinates
  coords2  :: a -> (BaseField a, BaseField a)
  -- | making a point from affine coordinates
  mkPoint2 :: (BaseField a, BaseField a) -> a   

--------------------------------------------------------------------------------
-- * Projective curves

class (Curve a, AffineCurve (AffinePoint a)) => ProjCurve a where
  -- | the corresponding affine curve
  type AffinePoint a :: *
  -- | convert from affine to projective coordinates
  fromAffine :: AffinePoint a -> a 
  -- | convert from projective to affine coordinates
  toAffine   :: a -> AffinePoint a
  -- | projective coordinates
  coords3  :: a -> (BaseField a, BaseField a, BaseField a)
  -- | making a point from projective coordinates
  mkPoint3 :: (BaseField a, BaseField a, BaseField a) -> a   
  -- | mixed addition
  mixedAdd :: a -> AffinePoint a -> a

--------------------------------------------------------------------------------
