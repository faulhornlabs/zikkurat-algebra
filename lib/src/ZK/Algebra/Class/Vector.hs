
-- | Polymorphic interface for 1-dimensional vectors

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeOperators #-}
module ZK.Algebra.Class.Vector where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy
import Data.Array
import Data.List
import Data.Kind

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.FFT

--------------------------------------------------------------------------------

-- | A sparse matrix
data SparseMatrix a = MkSparseMatrix
  { _sparseDims    :: !(Int,Int)
  , _sparseRowIdxs :: !(FlatArray Int)
  , _sparseColIdxs :: !(FlatArray Int)
  , _sparseCoeffs  :: !(FlatArray a  )
  }
  deriving Show

--------------------------------------------------------------------------------
-- * Pointwise operations

class PointwiseGroup a where
  -- | Pointwise negation
  pwNeg :: a -> a 
  -- | Pointwise addition
  pwAdd :: a -> a -> a
  -- | Pointwise subtraction
  pwSub :: a -> a -> a

class PointwiseGroup a => PointwiseRing a where
  -- | Pointwise squaring
  pwSqr :: a -> a
  -- | Pointwise multiplication
  pwMul :: a -> a -> a
  -- | Pointwise @a*b+c@
  pwMulAdd :: a -> a -> a -> a
  -- | Pointwise @a*b-c@
  pwMulSub :: a -> a -> a -> a

class PointwiseRing a => PointwiseField a where
  -- | Pointwise inversion
  pwInv :: a -> a
  -- | Pointwise division
  pwDiv :: a -> a -> a

--------------------------------------------------------------------------------
-- * 1 dimensional vectors 

{-
class Vector v where
  -- | Dimension of the vector
  vecSize :: v -> Int
  -- | The k-th element
  vecIndex :: Int -> v -> Element v
  -- | Append
  vecAppend :: v -> v -> v
  -- | Cons
  vecCons :: Element v -> v -> v
  -- | Snoc
  vecSnoc :: v -> Element v -> v 
  -- | constant vector
  constVector :: Int -> Element v -> v
  -- | eg. @basisVector' 0 1 k@ 
  basisVector' :: Element v -> Element v -> Int -> v
-}

-- | Finite dimensional vector spaces
class (WrappedArray v, Field (Element v), PointwiseField v) => VectorSpace v where
  -- | Dimension of the vector
  vecSize :: v -> Int
  -- | The k-th element
  vecIndex :: Int -> v -> Element v
  -- | Scaling by an element
  vecScale :: Element v -> v -> v
  -- | Dot product
  dotProd :: v -> v -> Element v
  -- | The array @[ a*b^k | k<-[0..n-1] ]@
  powers :: Element v -> Element v -> Int -> v
  -- | Pointwise multiplication by the array @[ a*b^k | k<-[0..n-1] ]@
  mulByPowers :: Element v -> Element v -> v -> v
  -- | Linear combination @a*x + y@
  linComb1 :: (Element v, v) -> v -> v
  -- | Linear combination @a*x + b*y@
  linComb2 :: (Element v, v) -> (Element v, v) -> v
  -- | Sparse matrix multiplication
  sparseMatrixMul :: SparseMatrix (Element v) -> v -> v
  -- | Append
  vecAppend :: v -> v -> v
  -- | Cons
  vecCons :: Element v -> v -> v
  -- | Snoc
  vecSnoc :: v -> Element v -> v 

{-
basisVector :: VectorSpace v => Int -> v
basisVector = basisVector' 0 1
-}

--------------------------------------------------------------------------------
