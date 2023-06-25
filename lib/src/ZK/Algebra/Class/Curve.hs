
{-# LANGUAGE BangPatterns, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
module ZK.Algebra.Class.Curve where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy

import ZK.Algebra.Class.Field

--------------------------------------------------------------------------------
-- * Finite groups

class (Eq a, Show a) => Group a where
  grpSize :: Proxy a -> Integer
  grpName :: Proxy a -> String
  grpUnit :: a
  grpNeg  :: a -> a
  grpDbl  :: a -> a
  grpAdd  :: a -> a -> a
  grpSub  :: a -> a -> a

--------------------------------------------------------------------------------
-- * Elliptic curves

class (Group a, Rnd a, Field (BaseField a), Field (ScalarField a)) => Curve a where
  -- | name of the ring
  curveNamePxy :: Proxy a -> String            
  -- | base field
  type BaseField a :: *
  -- | scalar field
  type ScalarField a :: *
  -- | check whther a point is on the curve
  isOnCurve   :: a -> Bool
  -- | check for being the point at infinity
  isInifinity :: a -> Bool
  -- | the infinity element
  infinity    :: a
  -- | the zero element
  scale :: ScalarField a -> a -> a

--------------------------------------------------------------------------------
