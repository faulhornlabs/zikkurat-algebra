
{-# LANGUAGE 
      BangPatterns, StrictData, ScopedTypeVariables, TypeFamilies, 
      TypeApplications, FlexibleContexts, FunctionalDependencies,
      PatternSynonyms, TypeOperators
  #-}
module ZK.Algebra.Pure.Field.Class where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Kind
import Data.Array
import Data.Bits
import Data.Word

import ZK.Algebra.Pure.Group
import ZK.Algebra.Pure.Misc

import {-# SOURCE #-} ZK.Algebra.Pure.Poly

import System.Random

--------------------------------------------------------------------------------

-- | A (finite) field
class (Eq f, Show f, Fractional f, SerializeMontgomery f, Rnd f) => Field f where
  -- | Name of the field
  fieldName      :: Proxy f -> String
  -- | the prime characteristic
  characteristic :: Proxy f -> Integer           
  -- | the dimension over the prime subfield
  dimension      :: Proxy f -> Int               
  -- | @q = p ^ dim@
  fieldSize      :: Proxy f -> Integer           
  -- | exponentiation
  pow            :: f -> Integer -> f
  -- | a primitive generator
  primGenPxy     :: Proxy f -> f                 
  -- | whether the input is the additive unit of the field
  isZero         :: f -> Bool                    
  -- | whether the input is the multiplicative unit of the field
  isOne          :: f -> Bool                    
  -- | the Frobenius endomorphism @\x -> x^p@
  frobenius      :: f -> f                       
  -- | project to the prime subfield
  projectToPrimeSubfield :: f -> Maybe Integer   

square :: Num a => a -> a
square x = x * x

inv :: Field f => f -> f
inv = recip

-- | A primitive generator
primGen :: Field f => f
primGen = primGenPxy Proxy

unsafeProjectToPrimeSubfield :: Field a => a -> Integer
unsafeProjectToPrimeSubfield x = case projectToPrimeSubfield x of
  Just y  -> y
  Nothing -> error "unsafeProjectToPrimeSubfield: element not in the prime subfield"

--------------------------------------------------------------------------------

-- | Prime fields
class Field f => PrimeField f where
  -- | The field element as an integer in the interval @[0,p-1]@
  asInteger :: f -> Integer       

--------------------------------------------------------------------------------
-- * Field extensions 

-- | Extension fields
class (Field f, Field (ExtBase f)) => ExtField f where
  -- | the base field
  type ExtBase f :: Type                               
  -- | the degree of the extension
  extDeg :: Proxy f -> Int                          
  -- | defining polynomial
  definingPoly :: Proxy f -> Poly (ExtBase f)
  -- | coefficients of the defining polynomial (starting from the constant term)
  definingPolyCoeffs :: Proxy f -> [ExtBase f]
  -- | embedding of the base field
  embedExtBase     :: ExtBase f -> f                
  -- | projection to the base field 
  projectToExtBase :: f -> Maybe (ExtBase f)        

  definingPoly pxy   = Poly $ listArray (0,extDeg pxy) (definingPolyCoeffs pxy)
  definingPolyCoeffs = polyCoeffList . definingPoly

-- | Quadratic extensions
class ExtField f => QuadraticExt f where
  quadraticMake   :: (ExtBase f, ExtBase f) -> f
  quadraticDecomp :: f -> (ExtBase f, ExtBase f)

-- | Cubic extensions
class ExtField f => CubicExt f where
  cubicMake   :: (ExtBase f, ExtBase f, ExtBase f) -> f
  cubicDecomp :: f -> (ExtBase f, ExtBase f, ExtBase f)

-------------------------------------------------------------------------------
-- * Fields with square root implemented

-- | Is a quadratic residue?
data QR
  = Residue
  | NonResidue
  | Zero
  deriving (Eq,Show)

-- | Square roots
data Sqrt a 
  = NoRoot
  | DblRoot  a
  | TwoRoots a a 
  deriving Show

-- | A square root of minus 1
newtype SqrtMinus1 f 
  = SqrtMinus1 f 
  deriving (Eq,Show)

class Field f => SqrtField f where
  isSquareRoot :: f -> QR
  fieldSqrt    :: f -> Maybe f

fieldSqrt2 :: SqrtField f => f -> Sqrt f
fieldSqrt2 x = case fieldSqrt x of
  Nothing -> NoRoot
  Just y1 -> let y2 = negate y1 in if y1 == y2 then DblRoot y1 else TwoRoots y1 y2

--------------------------------------------------------------------------------
-- * FFT-friendly fields

-- | A field with a large multiplicative cyclic subgroup with size power of two
class Field f => FFTField f where
  -- | generator of FFT domain
  domainGenPxy  :: Proxy f -> f         
  -- | logarithm of the size of the domain
  domainLogSize :: Proxy f -> Int       
  -- | actual size of the domain
  domainSize    :: Proxy f -> Int       

  domainSize pxy = 2 ^ domainLogSize pxy

domainGen :: FFTField f => f
domainGen = domainGenPxy Proxy

-- | Multiplicative subgroup of size @2^k@
domainSubgroup :: forall f. FFTField f => Int -> MulSubgroup f
domainSubgroup k 
  | k >= 0 && k <= n =  MulSG $ Subgroup 
        { subgroupGen   = pow (domainGenPxy (Proxy @f)) (2^(n-k))
        , subgroupOrder = (2^k)
        }
  | otherwise = error "domainSubgroup: domain size of out of possible range"
  where
    n = domainLogSize (Proxy @f)

--------------------------------------------------------------------------------
-- * Discrete logarithms

-- | The multiplicative subgroup of a finite field, encoded using the discrete logarithm
newtype DLog f = DLog Integer
  deriving (Eq,Show)

unDLog :: DLog f -> Integer
unDLog (DLog x) = x

toDLog :: forall f. Field f => Integer -> DLog f
toDLog n = DLog $ mod n (fieldSize (Proxy @f) - 1)

instance Field f => Num (DLog f) where
  negate (DLog x)       = DLog $ mod  x    (fieldSize (Proxy @f) - 1)
  (+) (DLog x) (DLog y) = DLog $ mod (x+y) (fieldSize (Proxy @f) - 1)
  (-) (DLog x) (DLog y) = DLog $ mod (x-y) (fieldSize (Proxy @f) - 1)
  (*) (DLog x) (DLog y) = DLog $ mod (x*y) (fieldSize (Proxy @f) - 1)
  fromInteger = toDLog
  signum _ = DLog 1
  abs dlog = dlog

instance Field f => Group (DLog f) where
  grpOrder _ = fieldSize (Proxy @f) - 1
  grpUnit    = toDLog 0
  grpNeg     = negate
  grpAdd     = (+)
  grpScale k x = toDLog k * x

instance (Field f) => Rnd (DLog f) where
  rndIO = DLog <$> randomRIO (0, fieldSize (Proxy @f) - 2) 

instance (Random f, Field f) => Random (DLog f) where
  random  g = let (a,g1) = randomR (0, fieldSize (Proxy @f) - 2) g in (DLog a, g1)
  randomR _ = error "DLog/Random/randomR: not implemented"

exponential :: Field f => DLog f -> f
exponential (DLog e) = pow primGen e

powl :: Field f => f -> DLog f -> f
powl x (DLog e) = pow x e

--------------------------------------------------------------------------------
-- * Multiplicitive subgroups of fields

-- | A multiplicative (cyclic) subgroup
newtype MulSubgroup f
  = MulSG (Subgroup f)
  deriving (Eq,Show)

pattern MulSubgroup gen size = MulSG (Subgroup gen size)

mulSubgroupGen :: MulSubgroup f -> f
mulSubgroupGen (MulSG s) = subgroupGen s

mulSubgroupOrder :: MulSubgroup f -> Int
mulSubgroupOrder (MulSG s) = subgroupOrder s

mulSubgroupElems :: forall f. Field f => MulSubgroup f -> [f]
mulSubgroupElems (MulSG (Subgroup gen order)) = go 1 order where
  go :: f -> Int -> [f]
  go _ 0 = []
  go a n = a : go (a * gen) (n-1)

-- halveMulSubgroup :: MulSubgroup f -> MulSubgroup f
-- halveMulSubgroup (MulSubgroup s) = MulSubgroup (halveSubgroup s)

-- have to repeat the code because the squaring (@Field@ is not an instance of @Group@)
halveMulSubgroup :: Field f => MulSubgroup f -> MulSubgroup f
halveMulSubgroup (MulSubgroup gen size) = if (size .&. 1 == 0)
  then MulSubgroup (gen*gen) (shiftR size 1)
  else error "halveMulSubgroup: subgroup order not divisible by two"

--------------------------------------------------------------------------------
-- * Additive subgroups of fields

-- | An additive (cyclic) subgroup
newtype AddSubgroup f
  = AddSG (Subgroup f)
  deriving (Eq,Show)

pattern AddSubgroup gen size = AddSG (Subgroup gen size)

addSubgroupGen :: AddSubgroup f -> f
addSubgroupGen (AddSG s) = subgroupGen s

addSubgroupOrder :: AddSubgroup f -> Int
addSubgroupOrder (AddSG s) = subgroupOrder s

addSubgroupElems :: forall f. Field f => AddSubgroup f -> [f]
addSubgroupElems (AddSG (Subgroup gen order)) = go 0 order where
  go :: f -> Int -> [f]
  go _ 0 = []
  go a n = a : go (a + gen) (n-1)

-- halveAddSubgroup :: AddSubgroup f -> AddSubgroup f
-- halveAddSubgroup (AddSubgroup s) = AddSubgroup (halveSubgroup s)

-- have to repeat the code because the squaring (@Field@ is not an instance of @Group@)
halveAddSubgroup :: Field f => AddSubgroup f -> AddSubgroup f
halveAddSubgroup (AddSubgroup gen size) = if (size .&. 1 == 0)
  then AddSubgroup (gen+gen) (shiftR size 1)
  else error "halveAddSubgroup: subgroup order not divisible by two"

--------------------------------------------------------------------------------
