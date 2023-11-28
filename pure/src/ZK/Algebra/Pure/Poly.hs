
-- | Dense univariate polynomials

{-# LANGUAGE StrictData, BangPatterns, ScopedTypeVariables, DeriveFunctor #-}
module ZK.Algebra.Pure.Poly where

--------------------------------------------------------------------------------

import Data.List
import Data.Array
import Data.Array.ST (STArray)
import Data.Array.MArray (newArray, readArray, writeArray, thaw, freeze)
import Data.Proxy

import Control.Monad
import Control.Monad.ST.Strict

import System.Random

import ZK.Algebra.Pure.Group
import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Misc

-- import ZK.Algebra.Pure.Field       -- debugging only!

--------------------------------------------------------------------------------
-- * Univariate polynomials

-- | A dense univariate polynomial. The array index corresponds to the exponent.
newtype Poly a 
  = Poly (Array Int a) 
  deriving (Show,Functor)

instance (Num a, Eq a) => Eq (Poly a) where
  p == q = polyIsZero (polySub p q)

mkPoly :: [a] -> Poly a
mkPoly coeffs = Poly $ listArray (0,length coeffs-1) coeffs

-- | Degree of the polynomial 
polyDegree :: (Eq a, Num a) => Poly a -> Int
polyDegree (Poly arr) = worker d0 where
  (0,d0) = bounds arr
  worker d 
    | d < 0        = -1
    | arr!d /= 0   =  d
    | otherwise    = worker (d-1)

polyIsZero :: (Eq a, Num a) => Poly a -> Bool
polyIsZero (Poly arr) = all (==0) (elems arr)

-- | Returns the coefficient of @x^k@
polyCoeff :: Num a => Poly a -> Int -> a
polyCoeff (Poly coeffs) k = safeIndex 0 coeffs k

-- | Note: this can include zero coeffs at higher than the actual degree!
polyCoeffArray :: Poly a -> Array Int a
polyCoeffArray (Poly coeffs) = coeffs

-- | Note: this cuts off the potential extra zeros at the end. 
-- The order is little-endian (constant term first).
polyCoeffList :: (Eq a, Num a) => Poly a -> [a]
polyCoeffList poly@(Poly arr) = take (polyDegree poly + 1) (elems arr)

--------------------------------------------------------------------------------
-- * Elementary polynomials

-- | Constant polynomial
polyConst :: a -> Poly a
polyConst x = Poly $ listArray (0,0) [x]

-- | Zero polynomial
polyZero :: Num a => Poly a
polyZero = polyConst 0

-- | The polynomial @f(x) = x@
polyVarX :: Num a => Poly a
polyVarX = mkPoly [0,1]

-- | @polyLinear (A,B)@ means the linear polynomial @f(x) = A*x + B@
polyLinear :: (a,a) -> Poly a
polyLinear (a,b) = mkPoly [b,a]

-- | The monomial @x^n@
polyXpowN :: Num a => Int -> Poly a
polyXpowN n = Poly $ listArray (0,n) (replicate n 0 ++ [1])

-- | The binomial @(x^n - 1)@
polyXpowNminus1 :: Num a => Int -> Poly a
polyXpowNminus1 n = Poly $ listArray (0,n) (-1 : replicate (n-1) 0 ++ [1])

--------------------------------------------------------------------------------
-- * Evaluate polynomials

polyEvalAt :: forall f. Field f => Poly f -> f -> f
polyEvalAt (Poly arr) x = go 0 1 0 where
  (0,d) = bounds arr
  go :: f -> f -> Int -> f
  go !acc !y !i = if i > d 
    then acc 
    else go (acc + (arr!i)*y) (y*x) (i+1)

polyEvalOnList :: forall f. Field f => Poly f -> [f] -> [f]
polyEvalOnList poly = map (polyEvalAt poly)

--------------------------------------------------------------------------------
-- * Vanishing polynomials

vanishingPoly :: Field f => MulSubgroup f -> Poly f
vanishingPoly subgroup = polyXpowNminus1 (mulSubgroupOrder subgroup)

divideByVanishingPoly :: Field f => MulSubgroup f -> Poly f -> (Poly f, Poly f)
divideByVanishingPoly subgroup poly = polyDivideBy_XpowNminus1 poly (mulSubgroupOrder subgroup)

divideByVanishingPoly_ :: Field f => MulSubgroup f -> Poly f -> Maybe (Poly f)
divideByVanishingPoly_ subgroup poly = polyDivideBy_XpowNminus1_ poly (mulSubgroupOrder subgroup)

--------------------------------------------------------------------------------
-- * Basic arithmetic operations on polynomials

polyNeg :: Num a => Poly a -> Poly a
polyNeg (Poly arr) = Poly $ fmap negate arr

polyAdd :: Num a => Poly a -> Poly a -> Poly a
polyAdd (Poly arr1) (Poly arr2) = Poly $ listArray (0,d3) zs where
  (0,d1) = bounds arr1
  (0,d2) = bounds arr2
  d3 = max d1 d2
  zs = zipWith (+) (elems arr1 ++ replicate (d3-d1) 0)
                   (elems arr2 ++ replicate (d3-d2) 0)

polySub :: Num a => Poly a -> Poly a -> Poly a
polySub (Poly arr1) (Poly arr2) = Poly $ listArray (0,d3) zs where
  (0,d1) = bounds arr1
  (0,d2) = bounds arr2
  d3 = max d1 d2
  zs = zipWith (-) (elems arr1 ++ replicate (d3-d1) 0)
                   (elems arr2 ++ replicate (d3-d2) 0)

polyMul :: Num a => Poly a -> Poly a -> Poly a
polyMul (Poly arr1) (Poly arr2) = Poly $ listArray (0,d3) zs where
  (0,d1) = bounds arr1
  (0,d2) = bounds arr2
  d3 = d1 + d2
  zs = [ f k | k<-[0..d3] ]
  f !k = foldl' (+) 0 [ arr1!i * arr2!(k-i) | i<-[ max 0 (k-d2) .. min d1 k ] ]

instance Num a => Num (Poly a) where
  fromInteger = polyConst . fromInteger
  negate = polyNeg
  (+)    = polyAdd
  (-)    = polySub
  (*)    = polyMul
  abs    = id
  signum = \_ -> polyConst 1

polySum :: Num a => [Poly a] -> Poly a
polySum = foldl' polyAdd 0

polyProd :: Num a => [Poly a] -> Poly a
polyProd = foldl' polyMul 1

--------------------------------------------------------------------------------
-- * Polynomial long division

-- | @polyDiv f h@ returns @(q,r)@ such that @f = q*h + r@ and @deg r < deg h@
polyDiv :: forall f. Field f => Poly f -> Poly f -> (Poly f, Poly f)
polyDiv poly_f@(Poly arr_f) poly_h@(Poly arr_h) 
  | deg_q < 0   =  (polyZero, poly_f)
  | otherwise   =  runST action
  where
    deg_f = polyDegree poly_f
    deg_h = polyDegree poly_h
    deg_q = deg_f - deg_h

    -- inverse of the top coefficient of divisor
    b_inv = inv (arr_h ! deg_h)

    action :: forall s. ST s (Poly f, Poly f) 
    action = do
      p <- thaw arr_f           :: ST s (STArray s Int f)
      q <- newArray (0,deg_q) 0 :: ST s (STArray s Int f)
      forM_ [deg_q,deg_q-1..0] $ \k -> do
        top <- readArray p (deg_h + k)
        let y = b_inv * top
        writeArray q k y
        forM_ [0..deg_h] $ \j -> do
          a <- readArray p (j+k)
          writeArray p (j+k) (a - y*(arr_h!j))
      qarr <- freeze q
      rs   <- forM [0..deg_h-1] $ \i -> readArray p i
      let rarr = listArray (0,deg_h-1) rs
      return (Poly qarr, Poly rarr)

-- | Returns only the quotient
polyDivQuo :: Field f => Poly f -> Poly f -> Poly f
polyDivQuo f g = fst $ polyDiv f g

-- | Returns only the remainder
polyDivRem :: Field f => Poly f -> Poly f -> Poly f
polyDivRem f g = snd $ polyDiv f g

{-
testPolyDiv :: forall f. (Show f, Field f, Random f) => Proxy f -> Int -> IO () 
testPolyDiv _ deg = do
  p  <- randomPolyIO deg    :: IO (Poly f)
  dh <- randomRIO (1,deg+3) :: IO Int
  h  <- randomPolyIO dh     :: IO (Poly f)
  let (q,r) = polyDiv p h
  let rhs   = q * h + r
  let deg_r = polyDegree r
  if deg_r < dh && rhs == p 
    then putStrLn "OK"
    else do
      putStrLn $ "BAD!"
      putStrLn $ "p       = " ++ show p
      putStrLn $ "h       = " ++ show h
      putStrLn $ "q*h + r = " ++ show rhs
      putStrLn $ "q = " ++ show q
      putStrLn $ "r = " ++ show r
-}

--------------------------------------------------------------------------------
-- * Optimized long divisions

-- test_p :: Poly F31
-- test_p = Poly $ listArray (0,5) [6,5,4,3,2,1]

-- | Divide by @(x^n - 1)@. Returns @(q,r)@ such that @f(x) = (x^n-1)*q(x) + r(x))@
polyDivideBy_XpowNminus1 :: forall f. Field f => Poly f -> Int -> (Poly f, Poly f)
polyDivideBy_XpowNminus1 poly@(Poly as) n 
  | n < 1      = error "polyDivideByXpowN: expecting n > 0"
  | d < n      = (polyZero, Poly as)
  | otherwise  = (Poly bs , Poly cs)
  where
    d     = polyDegree poly
    two_n = n+n

    lkp :: Array Int f -> Int -> f
    lkp arr i = safeIndex 0 arr i 

    bs = array (0,d-n) [ ( j , as!(j+n) + lkp bs (j+n) ) | j <- [0..d-n] ]
    cs = array (0,n-1) [ ( i , as!i     + lkp bs i     ) | i <- [0..n-1] ]  

polyDivideBy_XpowNminus1_ :: Field f => Poly f -> Int -> Maybe (Poly f)
polyDivideBy_XpowNminus1_ f n = let (q,r) = polyDivideBy_XpowNminus1 f n in
  if polyIsZero r then Just q else Nothing

{-
testPolyDivideBy_XpowNminus1 :: forall f. (Show f, Field f, Random f) => Proxy f -> Int -> IO () 
testPolyDivideBy_XpowNminus1 _ deg = do
  p  <- randomPolyIO deg    :: IO (Poly f)
  n  <- randomRIO (1,deg+3) :: IO Int
  testPolyDivideBy_XpowNminus1_what p n

testPolyDivideBy_XpowNminus1_what :: forall f. (Show f, Field f, Random f) => Poly f -> Int -> IO () 
testPolyDivideBy_XpowNminus1_what p n = do
  let (q,r) = polyDivideBy_XpowNminus1 p n
  let z = polyXpowNminus1 n
  let rhs   = q * z + r
  let deg_r = polyDegree r
  if deg_r < n && rhs == p
    then putStrLn "OK"
    else do
      putStrLn $ "BAD!"
      putStrLn $ "p =       " ++ show p
      putStrLn $ "q*z + r = " ++ show rhs
      putStrLn $ "q = " ++ show q
      putStrLn $ "r = " ++ show r
      putStrLn $ "z = " ++ show z
      putStrLn $ "n = " ++ show n
-}

--------------------------------------------------------------------------------

-- | Given @x0@, we compute the quotient @q(x) := (f(x) - f(x0)) / (x - x0)@
-- and also the value @y0 = f(x0)@
--
polyEvalQuotient :: Field f => Poly f -> f -> (f, Poly f)
polyEvalQuotient poly x0 
  | x0 == 0   = case as of
                  (0:as')  -> (y0, mkPoly [ a - y0 | a <- as' ])
                  _        -> error "polyEvalQuotient: should not happen"
  | otherwise = (y0, mkPoly bs) 
  where
    as@(a0:as') = polyCoeffList poly
    x0inv = inv x0
    y0 = polyEvalAt poly x0
    b0 = (y0 - a0) * x0inv 
    bs = worker b0 as'
    worker bi []     = []
    worker bi (a:as) = bi : worker ((bi - a)*x0inv) as

polyEvalQuotient_ :: Field f => Poly f -> f -> Poly f
polyEvalQuotient_ poly x0 = snd (polyEvalQuotient poly x0)

{-
testPolyEvalQuotient :: forall f. (Show f, Field f, Random f) => Proxy f -> Int -> IO () 
testPolyEvalQuotient _ deg = do
  p  <- randomPolyIO deg :: IO (Poly f)
  x0 <- randomIO         :: IO f
  let (y0,q) = polyEvalQuotient p x0
  let lhs = p - polyConst y0
  let rhs = q * (polyVarX - polyConst x0)
  case lhs == rhs of
    True  -> putStrLn "OK"
    False -> do
      putStrLn $ "BAD!"
      putStrLn $ "p   = " ++ show p
      putStrLn $ "x0  = " ++ show x0
      putStrLn $ "y0  = " ++ show y0
      putStrLn $ "q   = " ++ show q
      putStrLn $ "lhs = " ++ show lhs
      putStrLn $ "rhs = " ++ show rhs
-}

--------------------------------------------------------------------------------
-- * Sample random polynomials

randomPoly :: (RandomGen g, Random a) => Int -> g -> (Poly a, g)
randomPoly deg g0 = 
  let (coeffs,gfinal) = worker (deg+1) g0 
      poly = Poly (listArray (0,deg) coeffs)
  in  (poly, gfinal) 

  where
    worker 0 g = ([] , g)
    worker n g = let (x ,g1) = random g 
                     (xs,g2) = worker (n-1) g1
                 in  ((x:xs) , g)

randomPolyIO :: (Random a) => Int -> IO (Poly a)
randomPolyIO deg = getStdRandom (randomPoly deg)

--------------------------------------------------------------------------------
