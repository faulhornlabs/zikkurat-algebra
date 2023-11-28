
-- | Cubic equations and their root multiplicities

{-# LANGUAGE StrictData, BangPatterns #-}
module ZK.Algebra.Pure.Curve.Cubic where

--------------------------------------------------------------------------------

import ZK.Algebra.Pure.Field.Class

--------------------------------------------------------------------------------
-- * Cubic equations

-- | general cubic equation: @A*x^3 + B*x^2 + C*x + D == 0@
data GeneralCubic a 
  = GeneralCubic a a a a
  deriving (Show)

-- | Multiplicity of the given root of a cubic equation (can be one of 0,1,2,3)
generalCubicRootMult :: Field a => GeneralCubic a -> a -> Int
generalCubicRootMult genCubic x  = case reductionToDepresssedCubic (genCubic, x) of
  (Left  quadr , x) -> generalQuadraticRootMult quadr x
  (Right cubic , x) -> depressedCubicRootMult   cubic x

-- | Reduce to a depressed cubic (if it has indeed degree 3) or returns a general quadratic
-- (if the input has degree less than 3)
reductionToDepresssedCubic :: Field a => (GeneralCubic a , a) -> (Either (GeneralQuadratic a) (DepressedCubic a) , a)
reductionToDepresssedCubic (GeneralCubic a b c d , x) 
  | a == 0      = (Left  (GeneralQuadratic b c d) , x)       -- it's not really a cubic equation
  | otherwise   = (Right (DepressedCubic   p q  ) , y)
  where
    ofs = b / (3*a)
    y  = x + ofs
    a2 = a*a
    b2 = b*b
    a3 = a2*a
    b3 = b2*b
    p = (3*a*c - b2) / (3*a2)
    q = (2*b3 - 9*a*b*c + 27*a2*d) / (27*a3)

-- | Depressed cubic equation: @x^3 + P*x + Q == 0@
data DepressedCubic a 
  = DepressedCubic a a 
  deriving (Show)

depressedCubicDiscr :: Field a => DepressedCubic a -> a
depressedCubicDiscr (DepressedCubic p q) = 4*p*p*p + 27*q*q

depressedCubicRootMult :: Field a => DepressedCubic a -> a -> Int
depressedCubicRootMult cubic@(DepressedCubic p q) x 
  | x3 + p*x + q /= 0       = 0
  | discriminant /= 0       = 1
  | p == 0                  = 3
  | x == x_double           = 2
  | otherwise               = 1
  where
    x2 = x  * x
    x3 = x2 * x
    discriminant = 4*p*p*p + 27*q*q
    x_double     = - (3*q) / (2*p)       -- the double root

--------------------------------------------------------------------------------
-- * Quadratic equations

-- | general quadratic equation: @A*x^2 + B*x + C == 0@
data GeneralQuadratic a
  = GeneralQuadratic a a a
  deriving (Show)

-- | Multiplicity of the given root of a quadratic equation (can be one of 0,1,2)
generalQuadraticRootMult :: Field a => GeneralQuadratic a -> a -> Int
generalQuadraticRootMult (GeneralQuadratic a b c) x 
  | a == 0               = if b*x + c == 0 then 1 else 0     -- it's a linear equation
  | a*x2 + b*x + c /= 0  = 0                                 -- it's not a root
  | otherwise            = if discr == 0 then 2 else 1       -- it's a double root iff the discriminant is zero
  where
    x2    = x*x
    discr = b*b - 4*a*c  

--------------------------------------------------------------------------------
