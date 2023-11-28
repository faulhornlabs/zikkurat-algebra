
module ZK.Algebra.Pure.Poly where

import Data.Array

-- | A dense univariate polynomial. The array index corresponds to the exponent.
newtype Poly a 
  = Poly (Array Int a) 

mkPoly :: [a] -> Poly a

polyCoeffList :: (Eq a, Num a) => Poly a -> [a]