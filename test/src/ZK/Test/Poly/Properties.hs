
-- | Property tests for (univariate) polynomials

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Poly.Properties where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Array
import Data.List

import Control.Monad
import System.IO

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Poly

--------------------------------------------------------------------------------

allRingProps :: [PolyProp]
allRingProps = genericRingProps ++ specificPolyProps

runPolyTests :: forall a. Univariate a => Int -> Proxy a -> IO ()
runPolyTests n pxy = do

  forM_ allRingProps $ \prop -> case prop of
  
    PolyProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    PolyProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    PolyProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

    PolyPropF1 test name -> doTests n name $ do
      c <- rndIO @(Coeff a)
      x <- rndIO @a
      return (test c x) 

--------------------------------------------------------------------------------

doTests :: Int -> String -> IO Bool -> IO Bool
doTests n name testAction = 
  do
    let str = " - " ++ name ++ "... " 
    putStr $ str ++ replicate (30 - length str) ' '
    hFlush stdout
    oks <- forM [1..n] $ \i -> testAction
    let ok = and oks
    case ok of
      True  -> putStrLn $ "ok (passed " ++ show n ++ " tests)"
      False -> putStrLn $ "FAILED!! (FAILED " ++ show (countFalses oks) ++ " tests!)"
    return ok
  where
    countFalses :: [Bool] -> Int
    countFalses = length . filter (==False)

--------------------------------------------------------------------------------

data PolyProp
  = PolyProp1  (forall a. Univariate a  => a -> Bool           ) String
  | PolyProp2  (forall a. Univariate a  => a -> a -> Bool      ) String
  | PolyProp3  (forall a. Univariate a  => a -> a -> a -> Bool ) String
  | PolyPropF1 (forall a. Univariate a  => Coeff a -> a -> Bool) String

--------------------------------------------------------------------------------

genericRingProps :: [PolyProp]
genericRingProps = 
  [ PolyProp1 prop_add_left_unit               "add left unit"
  , PolyProp1 prop_add_right_unit              "add right unit"
  , PolyProp1 prop_add_left_inv                "add left inv"
  , PolyProp1 prop_add_right_inv               "add right inv"
  , PolyProp2 prop_add_commutative             "add comm"
  , PolyProp3 prop_add_associative             "add assoc"
  , PolyProp2 prop_sub_def                     "sub def"
  , PolyProp3 prop_add_sub_associative_1       "add-sub assoc /1"
  , PolyProp3 prop_add_sub_associative_2       "add-sub assoc /2"
  , PolyProp3 prop_add_sub_associative_3       "add-sub assoc /3"
  , PolyProp1 prop_is_zero                     "is zero"
  , PolyProp1 prop_is_one                      "is one"
  , PolyProp1 prop_is_equal                    "is equal"
  , PolyProp1 prop_mul_left_unit               "mul left unit"
  , PolyProp1 prop_mul_right_unit              "mul right unit"
  , PolyProp2 prop_mul_commutative             "mul comm"
  , PolyProp3 prop_mul_associative             "mul assoc"
  , PolyProp3 prop_add_mul_left_distributive   "add+mul left distr"
  , PolyProp3 prop_add_mul_right_distributive  "add+mul right distr"
  , PolyProp3 prop_sub_mul_left_distributive   "sub+mul left distr"
  , PolyProp3 prop_sub_mul_right_distributive  "sub+mul right distr"
  ]

specificPolyProps :: [PolyProp]
specificPolyProps = 
  [ PolyProp1  prop_neg_vs_ref          "neg vs. reference" 
  , PolyProp2  prop_add_vs_ref          "add vs. reference" 
  , PolyProp2  prop_sub_vs_ref          "sub vs. reference" 
  , PolyPropF1 prop_scale_vs_ref        "scale vs. reference" 
  , PolyProp2  prop_mul_vs_ref          "mul vs. reference" 
  ]

--------------------------------------------------------------------------------
-- * Ring properties

prop_add_left_unit :: Ring a => a -> Bool
prop_add_left_unit x = zero + x == x

prop_add_right_unit :: Ring a => a -> Bool
prop_add_right_unit x = x + zero == x

prop_add_left_inv :: Ring a => a -> Bool
prop_add_left_inv x = (negate x) + x == zero

prop_add_right_inv :: Ring a => a -> Bool
prop_add_right_inv x = x + (negate x) == zero

prop_add_commutative :: Ring a => a -> a -> Bool
prop_add_commutative x y = (x + y == y + x)

prop_add_associative :: Ring a => a -> a -> a -> Bool
prop_add_associative x y z = ((x + y) + z) == (x + (y + z))

prop_sub_def :: Ring a => a -> a -> Bool
prop_sub_def x y = (x + (negate y) == x - y)

prop_add_sub_associative_1 :: Ring a => a -> a -> a -> Bool
prop_add_sub_associative_1 x y z = ((x + y) - z) == (x + (y - z))

prop_add_sub_associative_2 :: Ring a => a -> a -> a -> Bool
prop_add_sub_associative_2 x y z = ((x - y) + z) == (x - (y - z))

prop_add_sub_associative_3 :: Ring a => a -> a -> a -> Bool
prop_add_sub_associative_3 x y z = ((x - y) - z) == (x - (y + z))

----------------------------------------

prop_is_zero :: forall a. Ring a => a -> Bool
prop_is_zero x = isZero (zero @a) && isZero x == (x == 0)

prop_is_one :: forall a. Ring a => a -> Bool
prop_is_one x = isOne (one @a) && isOne x == (x == 1)

prop_is_equal :: forall a. Ring a => a -> Bool
prop_is_equal x = and
  [ zero  == zero @a
  , zero  /= one  @a
  , one   /= zero @a
  , one   == one  @a
  , x     == x 
  , (x+1) /= x    
  , x     /= (x+1)
  ]

----------------------------------------

prop_mul_left_unit :: Ring a => a -> Bool
prop_mul_left_unit x = one * x == x

prop_mul_right_unit :: Ring a => a -> Bool
prop_mul_right_unit x = x * one == x

prop_mul_commutative :: Ring a => a -> a -> Bool
prop_mul_commutative x y = (x * y == y * x)

prop_mul_associative :: Ring a => a -> a -> a -> Bool
prop_mul_associative x y z = ((x * y) * z) == (x * (y * z))

----------------------------------------

prop_add_mul_left_distributive :: Ring a => a -> a -> a -> Bool
prop_add_mul_left_distributive x y z = (x + y) * z ==  x*z + y*z

prop_add_mul_right_distributive :: Ring a => a -> a -> a -> Bool
prop_add_mul_right_distributive x y z = x * (y + z) ==  x*y + x*z

prop_sub_mul_left_distributive :: Ring a => a -> a -> a -> Bool
prop_sub_mul_left_distributive x y z = (x - y) * z ==  x*z - y*z

prop_sub_mul_right_distributive :: Ring a => a -> a -> a -> Bool
prop_sub_mul_right_distributive x y z = x * (y - z) ==  x*y - x*z

--------------------------------------------------------------------------------
-- * Reference implementations

longZipWith :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
longZipWith f x0 y0 = go where
  go (x:xs) (y:ys) = f x  y  : go xs ys
  go (x:xs) []     = f x  y0 : go xs []
  go []     (y:ys) = f x0 y  : go [] ys
  go []     []     = []

negRef :: Univariate p => p -> p
negRef p = mkPoly $ map negate $ coeffs p

addRef :: Univariate p => p -> p -> p
addRef p q = mkPoly $ longZipWith (+) 0 0 (coeffs p) (coeffs q)

subRef :: Univariate p => p -> p -> p
subRef p q = mkPoly $ longZipWith (-) 0 0 (coeffs p) (coeffs q)

scaleRef :: Univariate p => Coeff p -> p -> p
scaleRef c p = mkPoly $ map (*c) $ coeffs p

mulRef :: Univariate p => p -> p -> p
mulRef p q = mkPoly list where
  parr = coeffsArr p
  qarr = coeffsArr q
  (0,d1) = bounds parr
  (0,d2) = bounds qarr
  list  = [ sum [ parr!i * qarr!j 
                | i<-[0..k]
                , let j=k-i
                , i<=d1
                , j>=0
                , j<=d2
                ] 
          | k<-[0..d1+d2] 
          ]

--------------------------------------------------------------------------------
-- * Polynomial properties

prop_neg_vs_ref :: Univariate p => p -> Bool
prop_neg_vs_ref p = (negate p == negRef p)

prop_add_vs_ref :: Univariate p => p -> p -> Bool
prop_add_vs_ref p q = (p + q == addRef p q)

prop_sub_vs_ref :: Univariate p => p -> p -> Bool
prop_sub_vs_ref p q = (p - q == subRef p q)

prop_scale_vs_ref :: Univariate p => Coeff p -> p -> Bool
prop_scale_vs_ref c p = (scale c p == scaleRef c p)

prop_mul_vs_ref :: Univariate p => p -> p -> Bool
prop_mul_vs_ref p q = (p * q == mulRef p q)

--------------------------------------------------------------------------------
