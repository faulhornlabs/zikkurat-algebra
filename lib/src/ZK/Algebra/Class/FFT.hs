
-- | FFT-friendly fields

{-# LANGUAGE StrictData, ScopedTypeVariables, TypeApplications #-}
module ZK.Algebra.Class.FFT where

--------------------------------------------------------------------------------

import ZK.Algebra.Class.Field

--------------------------------------------------------------------------------

-- | An FFT-friendly cyclic subgroup of the multiplicative group of a finite field
data FFTSubgroup a = MkFFTSubgroup
  { subgroupGen     :: !a        -- ^ the generator of the subgroup
  , subgroupLogSize :: !Int      -- ^ @log2@ of the size of the subgroup
  }
  deriving (Eq,Show)
  
-- | The size of the subgroup
subgroupSize :: FFTSubgroup a -> Int
subgroupSize sg = 2^(subgroupLogSize sg)

-- | Elements of the subgroup in standard order:
--
-- > [ 1, g, g^2, g^3, ... g^(n-1) ]
--
enumerateSubgroup :: forall a. Field a => FFTSubgroup a -> [a]
enumerateSubgroup (MkFFTSubgroup g m) = go (2^m) 1 where
  go :: Int -> a -> [a] 
  go 0 _ = []
  go k x = x : go (k-1) (g*x)

-- | The subgroup half the size, generated by @g^2@
halveSubgroup :: Field a => FFTSubgroup a -> FFTSubgroup a 
halveSubgroup (MkFFTSubgroup gen k)
  | k <= 0    = error "halveSubgroup: it's already the trivial subgroup"
  | otherwise = MkFFTSubgroup (square gen) (k-1)

--------------------------------------------------------------------------------

class Field a => FFTField a where
  -- | The largest power-of-two multiplicative subgroup the field supports
  fftDomain :: FFTSubgroup a      

-- | Given @n@, we construct the subgroup of size @2^n@
getFFTSubgroup :: forall a. FFTField a => Int -> FFTSubgroup a
getFFTSubgroup k 
  | k < 0     = error $ "fftSubGroup: expecting a nonnegative input (desired logarithmic size)"
  | k > m     = error $ "fftSubGroup: this field supports FFT subgroups of size at most " ++ show m
  | otherwise = let g = power gen (2^(m-k)) 
                in  MkFFTSubgroup g k
  where
    domain@(MkFFTSubgroup gen m) = fftDomain @a

--------------------------------------------------------------------------------
