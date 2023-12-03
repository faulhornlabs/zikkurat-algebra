
-- | Unsafe implementation of field extensions.
--
-- We encode the extension field elements as little-endian lists.
--
-- This module is considered internal.
--

{-# OPTIONS_HADDOCK hide #-} 
{-# LANGUAGE Strict, BangPatterns, ScopedTypeVariables #-}
module ZK.Algebra.Pure.Field.Raw.Extension where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List

import System.Random

--------------------------------------------------------------------------------
-- * helpers

mbCons :: Maybe a -> [a] -> [a]
mbCons (Just x) ys = x:ys
mbCons Nothing  ys =   ys

longZipWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
longZipWith x0 y0 f = go where
  go (x:xs) (y:ys) = f x y : go xs ys
  go []     []     = []
  go xs     []     = map (\x -> f x  y0) xs
  go []     ys     = map (\y -> f x0 y ) ys

-- example: @mapM_ print $ convolveWith (,) [1..5] [100..103]@
convolveWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [[c]]
convolveWith f = go where

  go :: [a] -> [b] -> [[c]]
  go []     ys         = [[]]
  go xs     []         = [[]]
  go (x:xs) yys@(y:ys) = this : rest where

    this :: [c]
    this = [f x y] :: [c]

    rest :: [[c]]
    rest = longZipWith Nothing [] mbCons (map (Just . f x) ys) (go xs yys)

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

--------------------------------------------------------------------------------
-- * polynomials over the base field

polyConstTerm :: Num a => [a] -> a
polyConstTerm []    = 0
polyConstTerm (x:_) = x

polyDegree :: (Eq a, Num a) => [a] -> Int
polyDegree = go (-1) 0 where
  go sofar d []     = sofar
  go sofar d (x:xs) = go (if x/=0 then d else sofar) (d+1) xs

polyEqual :: (Eq a, Num a) => [a] -> [a] -> Bool
polyEqual xs ys = and (longZipWith 0 0 (==) xs ys)

polyIsZero :: (Eq a, Num a) => [a] -> Bool
polyIsZero = all (==0)

polyIsOne :: (Eq a, Num a) => [a] -> Bool
polyIsOne [] =  False
polyIsOne (x:xs) = (x==1) && all (==0) xs

polyScale :: (Eq a, Num a) => a -> [a] -> [a]
polyScale 0 _  = []
polyScale s xs = map (*s) xs

polyNeg :: Num a => [a] -> [a]
polyNeg = map negate

polyAdd :: Num a => [a] -> [a] -> [a]
polyAdd = longZipWith 0 0 (+)

polySub :: Num a => [a] -> [a] -> [a]
polySub = longZipWith 0 0 (-)

polyMul :: Num a => [a] -> [a] -> [a]
polyMul xs ys = map sum' $ convolveWith (*) xs ys

polyLongDiv :: (Eq a, Fractional a) => [a] -> [a] -> ([a],[a])
polyLongDiv xs0 ys0
  | polyIsZero ys0  = error "polyLongDiv: division by zero polynomial"
  | polyIsZero xs0  = ([],[])
  | otherwise       = let (q,r) = go n0 [] (reverse xs0) in (q, reverse r)
  where
    n0 = length xs0
    m0 = length ys + 1
    (y:ys) = dropWhile (==0) $ reverse ys0
    go !n q []           = (q,[])
    go !n q ((!x):(!xs)) = if n < m0 
      then (q,(x:xs))
      else let c = x / y
           in  go (n-1) (c:q) $ zipWith (-) xs (map (*c) ys ++ repeat 0)

polyLongDivQuot :: (Eq a, Fractional a) => [a] -> [a] -> [a]
polyLongDivQuot xs ys = fst $ polyLongDiv xs ys

polyLongDivRem :: (Eq a, Fractional a) => [a] -> [a] -> [a]
polyLongDivRem xs ys = snd $ polyLongDiv xs ys

--------------------------------------------------------------------------------
-- * field operations

extEq :: (Eq a, Num a) => [a] -> [a] -> Bool
extEq = polyEqual

extNeg :: Num a => [a] -> [a]
extNeg = polyNeg

extAdd :: Num a => [a] -> [a] -> [a]
extAdd = polyAdd

extSub :: Num a => [a] -> [a] -> [a]
extSub = polySub

extInj :: Num a => Int -> a -> [a]
extInj n x = x : replicate (n-1) 0

extMul :: (Eq a, Fractional a) => [a] -> [a] -> [a] -> [a]
extMul irred_poly xs ys = polyLongDivRem (polyMul xs ys) irred_poly where

extDiv :: (Eq a, Fractional a) => [a] -> [a] -> [a] -> [a]
extDiv irred xs ys = extMul irred xs (extInv irred ys)

extRnd :: (Random a, RandomGen g) => Int -> g -> ([a],g)
extRnd = worker where
  worker 0 g  = ([],g)
  worker n g0 = 
    let (x   , g1) = random       g0
        (xs  , g2) = worker (n-1) g1 
    in  (x:xs, g2)

--------------------------------------------------------------------------------
-- * power

extPow :: forall a. (Eq a, Fractional a) => [a] -> [a] -> Integer -> [a]
extPow irred z e 
  | e < 0     = extPow irred (extInv irred z) (negate e)
  | null z    = [0]
  | e == 0    = [1]
  | otherwise = go [1] z e
  where

    go :: [a] -> [a] -> Integer -> [a]
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go               acc    (extMul irred y y) (shiftR e 1)
        _ -> go (extMul irred acc y) (extMul irred y y) (shiftR e 1)

--------------------------------------------------------------------------------
-- * inverse

-- based on <https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Simple_algebraic_field_extensions>
extInv :: forall a. (Eq a, Fractional a) => [a] -> [a] -> [a]
extInv irred a 
  | all (==0) a  = error "field inverse of zero (generic extension)"
  | otherwise    = worker [] [1] irred a 
  where

    worker :: [a] -> [a] -> [a] -> [a] -> [a] 
    worker t newt r newr = case polyIsZero newr of

      False -> let quot = polyLongDivQuot r newr 
                   r'   = polySub r (polyMul quot newr)
                   t'   = polySub t (polyMul quot newt)
               in  worker newt t' newr r'

      True  -> if (polyDegree r > 0)
        then []      -- either `irred` is not irredubile or `a` is zero (mod irred)
        else let r0 = polyConstTerm r
             in  polyScale (recip r0) t

--------------------------------------------------------------------------------
-- testing

{-

p1 = [101,102,103,104,105,106,107,0,0] :: [F14]
p2 = [303,304,305,306] :: [F14]
(q,r) = polyLongDiv p1 p2
s = polyMul q p2 `polyAdd` r

-- p = 12289
-- 5 - 4 x + x^2 + 7 x^3 + x^4
irred = [5, -4, 1, 7, 1] :: [F14]

a = [2,3,4,5]     :: [F14]
b = [10,11,12,13] :: [F14]
c = extMul irred a b
d = extPow irred c 1234567
e = extDiv irred a b

-}
