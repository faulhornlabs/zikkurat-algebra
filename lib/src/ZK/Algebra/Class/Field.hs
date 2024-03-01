
-- | Polymorphic interface for finite fields

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeApplications #-}
module ZK.Algebra.Class.Field where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy
import Data.Kind

import ZK.Algebra.Class.Flat ( Flat , FlatArray )
import ZK.Algebra.Class.Misc

--------------------------------------------------------------------------------
-- * Rings (finite rings and polynomials rings)

class (Eq a, Show a, Rnd a, Num a) => Ring a where
  -- | name of the ring
  ringName    :: Proxy a -> String            
  -- | size of the ring
  ringSize    :: Proxy a -> Integer           
  -- | check for being equal to zero
  isZero      :: a -> Bool
  -- | check for being equal to one
  isOne       :: a -> Bool
  -- | the zero element
  zero        :: a
  -- | the unit element
  one         :: a
  -- | squaring
  square      :: a -> a
  -- | exponentiation
  power       :: a -> Integer -> a      

power_ :: Ring a => a -> Int -> a
power_ x e = power x (fromIntegral e)

--------------------------------------------------------------------------------
-- * Finite fields

class (Flat a, Ring a, Fractional a) => Field a where
  -- | prime characteristic of the field
  characteristics  :: Proxy a -> Integer     
  -- | dimension of the field (over the prime field)
  dimension        :: Proxy a -> Int         
  -- | a fixed primitive element (generator of the multiplicative group)
  primGenPxy   :: Proxy a -> a           
  -- | efficient division by 2
  halve        :: a -> a
  -- | efficient batch inverse
  batchInverse :: FlatArray a -> FlatArray a
  -- | the Frobenius endomorphism @\x -> x^p@ where $p$ is the characteristic of the field
  frobenius    :: a -> a

-- | multiplicative inverse
inverse :: Field a => a -> a      
inverse = recip

fieldName :: Field a => Proxy a -> String
fieldName = ringName

fieldSize :: Field a => Proxy a -> Integer
fieldSize = ringSize

primGen :: forall a. Field a => a
primGen = primGenPxy (Proxy :: Proxy a)

----------------------------------------
-- * Converting between standard and Montgomery representation

-- | Fields in both standard and Montgomery representation
class (Field a, Field (StandardField a)) => MontgomeryField a where
  -- | the standard representation
  type StandardField a :: Type
  toStandardRep   :: a -> StandardField a
  fromStandardRep :: StandardField a -> a
  batchToStandardRep   :: FlatArray a -> FlatArray (StandardField a)
  batchFromStandardRep :: FlatArray (StandardField a) -> FlatArray a

--------------------------------------------------------------------------------
-- * Prime fields

class Field f => PrimeField f where
  -- | Convert to an integer between @0@ and @p-1@
  asInteger :: f -> Integer

fieldPrime :: PrimeField f => Proxy f -> Integer
fieldPrime = characteristics

--------------------------------------------------------------------------------
-- * Algebraic field extensions

-- | Extension fields
class (Field f, Field (ExtBase f)) => ExtField f where
  -- | the base field
  type ExtBase f :: Type                               
  -- | the degree of the extension
  extDeg :: Proxy f -> Int                          
  -- | coefficients of the defining polynomial (starting from the constant term)
  definingPolyCoeffs :: Proxy f -> [ExtBase f]
  -- | embedding of the base field
  embedExtBase     :: ExtBase f -> f                
  -- | projection to the base field 
  projectToExtBase :: f -> Maybe (ExtBase f)        
  -- | scale by an element of the base field
  scaleExtBase     :: ExtBase f -> f ->f
  -- | convert from a vector over the base field
  extPack :: [ExtBase f] -> f
  -- | convert to a vector over the base field
  extUnpack :: f -> [ExtBase f]

-- | Extension fields, interpreted as vector spaces over the /prime field/
class (Field f, PrimeField (PrimeBase f)) => ExtField' f where
  -- | the underlying prime field
  type PrimeBase f :: Type                               
  -- | embedding of the prime field
  embedPrimeField     :: PrimeBase f -> f                
  -- | projection to the prime field 
  projectToPrimeField :: f -> Maybe (PrimeBase f)        
  -- | scale by an element of the base field
  scalePrimeField     :: PrimeBase f -> f ->f
  -- | convert from a vector over the prime field
  primePack :: [PrimeBase f] -> f
  -- | convert to a vector over the prime field
  primeUnpack :: f -> [PrimeBase f]

--------------------------------------------------------------------------------

-- | Quadratic extensions
class ExtField f => QuadraticExt f where
  quadraticPack   :: (ExtBase f, ExtBase f) -> f
  quadraticUnpack :: f -> (ExtBase f, ExtBase f)

-- | Cubic extensions
class ExtField f => CubicExt f where
  cubicPack   :: (ExtBase f, ExtBase f, ExtBase f) -> f
  cubicUnpack :: f -> (ExtBase f, ExtBase f, ExtBase f)

--------------------------------------------------------------------------------

-- | Generic exponentiation for rings
ringPowerDefault :: forall r. (Ring r) => r -> Integer -> r
ringPowerDefault !z !e 
  | isZero z  = z 
  | e == 0    = one
  | e < 0     = error "powerRingDefault: negative powers do not make sense"
  | otherwise = go one z e
  where
    go :: r -> r -> Integer -> r
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

-- | Generic exponentiation for fields
fieldPowerDefault :: forall f. (Field f) => f -> Integer -> f
fieldPowerDefault !z !e 
  | isZero z  = z 
  | e == 0    = one
  | e < 0     = fieldPowerDefault (recip z) (negate e)
  | e >= pm1  = go one z (mod e pm1)
  | otherwise = go one z e
  where
    pm1 = fieldSize (Proxy :: Proxy f)
    go :: f -> f -> Integer -> f
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

-- | Naive generic Frobenius automorphism (for testing purposes)
frobeniusNaive :: forall f. (Field f) => f -> f
frobeniusNaive x = power x (characteristics (Proxy @f))

--------------------------------------------------------------------------------
