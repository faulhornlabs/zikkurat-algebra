
-- | Polymorphic interface for polynomials

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module ZK.Algebra.Class.Poly where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy
import Data.Array

import ZK.Algebra.Class.Field

--------------------------------------------------------------------------------
-- * Univariate polynomials over (finite) fields

class (Ring p, Field (Coeff p)) => Univariate p where
  -- | the type of coefficients
  type Coeff p :: *
  -- | Degree
  degree :: p -> Int
  -- | Coefficients
  coeffs :: p -> [Coeff p]
  -- | Coefficients as an Array
  coeffsArr :: p -> Array Int (Coeff p)
  -- | The k-th coefficient
  kthCoeff :: Int -> p -> Coeff p
  -- | Create a polynomial from coefficiens
  mkPoly :: [Coeff p] -> p
  -- | Evaluation
  evalAt :: Coeff p -> p -> Coeff p
  -- | Scaling
  scale :: Coeff p -> p -> p

--------------------------------------------------------------------------------
