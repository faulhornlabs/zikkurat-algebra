
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
-- * Projective curves

class Curve a => ProjCurve a where
  -- type AffinePoint a :: *
  -- fromAffine :: AffinePoint a -> a 
  -- toAffine   :: a -> AffinePoint a
  coords  :: a -> (BaseField a, BaseField a, BaseField a)
  mkPoint :: (BaseField a, BaseField a, BaseField a) -> a   

--------------------------------------------------------------------------------
