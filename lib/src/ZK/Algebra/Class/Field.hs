
-- | Polymorphic interface for finite fields

{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module ZK.Algebra.Class.Field where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy
import Data.Kind

import ZK.Algebra.Class.Flat ( Flat , FlatArray )

--------------------------------------------------------------------------------

class Rnd a where
  rndIO :: IO a
  
--------------------------------------------------------------------------------
-- * Rings (finite rings and polynomials rings)

class (Eq a, Show a, Rnd a, Num a) => Ring a where
  -- | name of the ring
  ringNamePxy :: Proxy a -> String            
  -- | size of the ring
  ringSizePxy :: Proxy a -> Integer           
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

--------------------------------------------------------------------------------
-- * Finite fields

class (Flat a, Ring a, Fractional a) => Field a where
  -- | prime characteristic of the field
  charPxy      :: Proxy a -> Integer     
  -- | dimension of the field (over the prime field)
  dimPxy       :: Proxy a -> Int         
  -- | a fixed primitive element (generator of the multiplicative group)
  primGenPxy   :: Proxy a -> a           
  -- | efficient batch inverse
  batchInverse :: FlatArray a -> FlatArray a

-- | multiplicative inverse
inverse :: Field a => a -> a      
inverse = recip

fieldNamePxy :: Field a => Proxy a -> String
fieldNamePxy = ringNamePxy

fieldSizePxy :: Field a => Proxy a -> Integer
fieldSizePxy = ringSizePxy

----------------------------------------

primGen :: forall a. Field a => a
primGen = primGenPxy (Proxy :: Proxy a)

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
    pm1 = fieldSizePxy (Proxy :: Proxy f)
    go :: f -> f -> Integer -> f
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go  acc    (y*y) (shiftR e 1)
        _ -> go (acc*y) (y*y) (shiftR e 1)

--------------------------------------------------------------------------------
