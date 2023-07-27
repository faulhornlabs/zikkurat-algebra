
-- | Polymorphic interface for (univariate) polynomials

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module ZK.Algebra.Class.Poly where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy
import Data.Array
import Data.List

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.FFT

--------------------------------------------------------------------------------
-- * Univariate polynomials over (finite) fields

class (Ring p, Field (Coeff p), WrappedArray p, Element p ~ Coeff p) => Univariate p where
  -- | the type of coefficients
  type Coeff p :: *
  -- | Degree
  degree :: p -> Int
  -- | The k-th coefficient
  kthCoeff :: Int -> p -> Coeff p
  -- | Evaluation
  evalAt :: Coeff p -> p -> Coeff p
  -- | Scaling
  scale :: Coeff p -> p -> p
  -- | Create a polynomial from coefficiens
  mkPoly :: [Coeff p] -> p
  -- | Coefficients of the polynomial as a list
  coeffs :: p -> [Coeff p]
  -- | Coefficients as an Array
  coeffsArr :: p -> Array Int (Coeff p)
  -- | Coefficients as a FlatArray
  coeffsFlatArr :: p -> FlatArray (Coeff p)
  -- | Polynomial long division
  polyLongDiv :: p -> p -> (p,p)
  -- | Polynomial quotient
  polyQuot :: p -> p -> p
  -- | Polynomial remainder
  polyRem :: p -> p -> p
  -- | Divide by the coset vanishing polynomial @(x^n - eta)@
  divByVanishing :: p -> (Int, Coeff p) -> (p,p)
  -- | Quotient by the coset vanishing polynomial @(x^n - eta)@
  quotByVanishing :: p -> (Int, Coeff p) -> Maybe p

-- | Polynomials which over an FFT-friend field support NTT operations
class (Univariate p, FFTField (Coeff p)) => UnivariateFFT p where
  -- | Number-theoretical transform (evaluate on a subgroup)
  ntt  :: FFTSubgroup (Coeff p) -> p -> FlatArray (Coeff p) 
  -- | Inverse number-theoretical transform (interpolate on a subgroup)
  intt :: FFTSubgroup (Coeff p) -> FlatArray (Coeff p) -> p

--------------------------------------------------------------------------------
-- * Some generic functions

-- | Checks whether the input is the constant one polynomial?
polyIsOne :: Univariate p => p -> Bool
polyIsOne p = case mbConst p of
  Nothing -> False
  Just x  -> ZK.Algebra.Class.Field.isOne x

-- | The constant term of a polynomial
constTermOf :: Univariate p => p -> Coeff p
constTermOf = kthCoeff 0

-- | Is this a constant polynomial? 
--
-- TODO: this is not efficient for high-degree polynomials, where we could exit early...
mbConst :: Univariate p => p -> Maybe (Coeff p)
mbConst p = if degree p <= 0 then Just (constTermOf p) else Nothing

-- | Create a constant polynomial
constPoly :: Univariate p => Coeff p -> p
constPoly y = mkPoly [y]

-- | The polynomial @p(x)=x@
idPoly :: Univariate p => p
idPoly = mkPoly [0,1]

-- | Create a linear polynomial.
-- 
-- > linearPoly a b == a*x + b@
--
linearPoly :: Univariate p => Coeff p -> Coeff p -> p
linearPoly a b = mkPoly [b,a]

showPoly :: Univariate p => p -> String
showPoly = showPoly' True

showPoly' :: Univariate p => Bool -> p -> String
showPoly' newlines_flag poly =
  case newlines_flag of
    False -> intercalate " +"   terms
    True  -> intercalate " +\n" terms
  where
    pairs  = filter (\kx -> snd kx /= 0) 
           $ zip [0..] (coeffs poly)
    terms  = case pairs of 
               [] -> [" 0"]
               _  -> map f pairs
    f (0,x) = ' ' : show x
    f (1,x) = ' ' : show x ++ " * x"
    f (k,x) = ' ' : show x ++ " * x^" ++ show k

--------------------------------------------------------------------------------
