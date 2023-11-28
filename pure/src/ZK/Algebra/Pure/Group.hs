
-- | Groups
--
-- Examples are multiplicative (sub)groups in finite fields, and
-- elliptic curve groups.
--

{-# LANGUAGE ScopedTypeVariables #-}
module ZK.Algebra.Pure.Group where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List
import Data.Proxy 

--------------------------------------------------------------------------------
-- * Groups

-- | We can write groups additively or multiplicatively, 
-- just be careful and don't mix the two :)
--
-- For simplicity the class methods are in additive notation, and
-- the multiplicative notation are just normal functions below.
--
class Group g where
  -- | The size of the group
  grpOrder :: Proxy g -> Integer
  -- | The unit element of the group
  grpUnit  :: g
  -- | The inverse operation in the group
  grpNeg   :: g -> g
  -- | The binary operation in the group
  grpAdd   :: g -> g -> g
  -- | Scaling by an integer
  grpScale :: Integer -> g -> g

grpSub :: Group g => g -> g -> g
grpSub x y = grpAdd x (grpNeg y)

grpSum :: Group g => [g] -> g
grpSum = foldl' grpAdd grpUnit

--------------------------------------------------------------------------------
-- * Multiplicative notation

mgrpUnit :: Group g => g
mgrpUnit = grpUnit

mgrpInv :: Group g => g -> g
mgrpInv = grpNeg

mgrpMul :: Group g => g -> g -> g
mgrpMul = grpAdd

mgrpDiv :: Group g => g -> g -> g
mgrpDiv = grpSub

mgrpExpo :: Group g => g -> Integer -> g
mgrpExpo = flip grpScale

mgrpProd :: Group g => [g] -> g
mgrpProd = grpSum

--------------------------------------------------------------------------------
-- * Cyclic subgroups

-- | A cyclic subgroup
data Subgroup g = Subgroup 
  { subgroupGen   :: g          -- ^ the cyclic generator 
  , subgroupOrder :: Int        -- ^ size of the subgroup
  }
  deriving (Eq,Show)

-- | lists all elements of the (cyclic) subgroup
subgroupElems :: forall g. Group g => Subgroup g -> [g]
subgroupElems (Subgroup gen order) = go grpUnit order where
  go :: g -> Int -> [g]
  go _ 0 = []
  go a n = a : go (grpAdd a gen) (n-1)

halveSubgroup :: Group g => Subgroup g -> Subgroup g
halveSubgroup (Subgroup gen size) = if (size .&. 1 == 0)
  then Subgroup (grpAdd gen gen) (shiftR size 1)
  else error "halveSubgroup: subgroup order not divisible by two"

--------------------------------------------------------------------------------
