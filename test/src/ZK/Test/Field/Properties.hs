
-- | Property tests for rings and fields

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Field.Properties where

--------------------------------------------------------------------------------

import Data.Proxy

import Control.Monad
import System.IO

import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Field

--------------------------------------------------------------------------------

runFieldTests :: forall a. Field a => Int -> Proxy a -> IO ()
runFieldTests n pxy = do
  runRingTests      n pxy
  runFieldOnlyTests n pxy 

runRingTests :: forall a. Ring a => Int -> Proxy a -> IO ()
runRingTests n pxy = do

  forM_ ringProps $ \prop -> case prop of
  
    RingProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    RingProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    RingProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

runFieldOnlyTests :: forall a. Field a => Int -> Proxy a -> IO ()
runFieldOnlyTests n pxy = do

  forM_ fieldOnlyProps $ \prop -> case prop of
  
    FieldProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    FieldProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    FieldProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

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

data RingProp
  = RingProp1  (forall a. Ring a  => a -> Bool          ) String
  | RingProp2  (forall a. Ring a  => a -> a -> Bool     ) String
  | RingProp3  (forall a. Ring a  => a -> a -> a -> Bool) String

data FieldProp
  = FieldProp1 (forall a. Field a => a -> Bool          ) String
  | FieldProp2 (forall a. Field a => a -> a -> Bool     ) String
  | FieldProp3 (forall a. Field a => a -> a -> a -> Bool) String

--------------------------------------------------------------------------------

ringProps :: [RingProp]
ringProps = 
  [ RingProp1 prop_add_left_unit               "add left unit"
  , RingProp1 prop_add_right_unit              "add right unit"
  , RingProp1 prop_add_left_inv                "add left inv"
  , RingProp1 prop_add_right_inv               "add right inv"
  , RingProp2 prop_add_commutative             "add comm"
  , RingProp3 prop_add_associative             "add assoc"
  , RingProp2 prop_sub_def                     "sub def"
  , RingProp3 prop_add_sub_associative_1       "add-sub assoc /1"
  , RingProp3 prop_add_sub_associative_2       "add-sub assoc /2"
  , RingProp3 prop_add_sub_associative_3       "add-sub assoc /3"
  , RingProp1 prop_is_zero                     "is zero"
  , RingProp1 prop_is_one                      "is one"
  , RingProp1 prop_is_equal                    "is equal"
  , RingProp1 prop_mul_left_unit               "mul left unit"
  , RingProp1 prop_mul_right_unit              "mul right unit"
  , RingProp2 prop_mul_commutative             "mul comm"
  , RingProp3 prop_mul_associative             "mul assoc"
  , RingProp1 prop_square_def                  "square def"
  , RingProp2 prop_square_distrib              "square distributive"
  , RingProp3 prop_add_mul_left_distributive   "add+mul left distr"
  , RingProp3 prop_add_mul_right_distributive  "add+mul right distr"
  , RingProp3 prop_sub_mul_left_distributive   "sub+mul left distr"
  , RingProp3 prop_sub_mul_right_distributive  "sub+mul right distr"
  , RingProp1 prop_power_0                     "0-th power"
  , RingProp1 prop_power_1                     "1-th power"
  , RingProp1 prop_power_2                     "2-th power"
  , RingProp1 prop_power_3                     "3-th power"
  , RingProp1 prop_power_4                     "4-th power"
  , RingProp1 prop_power_5                     "5-th power"
  ]

fieldOnlyProps :: [FieldProp]
fieldOnlyProps = 
  [ FieldProp1 prop_mul_left_inv               "mul left inf"
  , FieldProp1 prop_mul_right_inv              "mul right inf"
  , FieldProp2 prop_div_def                    "div def"
  , FieldProp1 prop_inv_def                    "inv def"
  , FieldProp2 prop_div_test                   "div defining prop."
  , FieldProp1 prop_inv_fermat                 "inv == fermat"
  , FieldProp1 prop_fermat_1                   "fermat/1"
  , FieldProp1 prop_fermat_2                   "fermat/2"
  , FieldProp1 prop_power_neg                  "negative power"
  , FieldProp3 prop_mul_div_associative_1      "mul-div assoc /1"
  , FieldProp3 prop_mul_div_associative_2      "mul-div assoc /2"
  , FieldProp3 prop_mul_div_associative_3      "mul-div assoc /3"
  , FieldProp3 prop_batch_inverse              "batch inverse"
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
prop_mul_left_unit x = (one * x == x)

prop_mul_right_unit :: Ring a => a -> Bool
prop_mul_right_unit x = (x * one == x)

prop_mul_commutative :: Ring a => a -> a -> Bool
prop_mul_commutative x y = (x * y == y * x)

prop_mul_associative :: Ring a => a -> a -> a -> Bool
prop_mul_associative x y z = ((x * y) * z) == (x * (y * z))

prop_square_def :: Ring a => a -> Bool
prop_square_def x = (square x == x*x)

prop_square_distrib :: Ring a => a -> a -> Bool
prop_square_distrib x y =  (square (x+y) == square x + 2*x*y + square y)
                        && (square (x-y) == square x - 2*x*y + square y)

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

prop_power_0 :: Ring a => a -> Bool
prop_power_0 x = power x 0 == (if x == 0 then zero else one)

prop_power_1 :: Ring a => a -> Bool
prop_power_1 x = power x 1 == x

prop_power_2 :: Ring a => a -> Bool
prop_power_2 x = power x 2 == x *x

prop_power_3 :: Ring a => a -> Bool
prop_power_3 x = power x 3 == x*x*x

prop_power_4 :: Ring a => a -> Bool
prop_power_4 x = power x 4 == x*x*x*x

prop_power_5 :: Ring a => a -> Bool
prop_power_5 x = power x 5 == x*x*x*x*x

--------------------------------------------------------------------------------
-- * Field properties

prop_mul_left_inv :: Field a => a -> Bool
prop_mul_left_inv x = isZero x || (inverse x) * x == one

prop_mul_right_inv :: Field a => a -> Bool
prop_mul_right_inv x = isZero x || x * (inverse x) == one

prop_div_def :: Field a => a -> a -> Bool
prop_div_def x y = (x * (inverse y) == x / y)

prop_inv_def :: Field a => a -> Bool
prop_inv_def x = (inverse x == 1 / x)

prop_div_test :: Field a => a -> a -> Bool
prop_div_test x y = isZero y || (x/y)*y == x

prop_inv_fermat :: forall a. Field a => a -> Bool
prop_inv_fermat x = (inverse x) == power x (p - 2) where p = charPxy (Proxy @a)

prop_fermat_1 :: forall a. Field a => a -> Bool
prop_fermat_1 x = power x p == x where p = charPxy (Proxy @a)

prop_fermat_2 :: forall a. Field a => a -> Bool
prop_fermat_2 x = power x (p - 1) == one where p = charPxy (Proxy @a)

prop_power_neg :: forall a. Field a => a -> Bool
prop_power_neg x = power x (-1) == inverse x

prop_mul_div_associative_1 :: Field a => a -> a -> a -> Bool
prop_mul_div_associative_1 x y z = ((x * y) / z) == (x * (y / z))

prop_mul_div_associative_2 :: Field a => a -> a -> a -> Bool
prop_mul_div_associative_2 x y z = ((x / y) * z) == (x / (y / z))

prop_mul_div_associative_3 :: Field a => a -> a -> a -> Bool
prop_mul_div_associative_3 x y z = ((x / y) / z) == (x / (y * z))

prop_batch_inverse :: Field a => a -> a -> a -> Bool
prop_batch_inverse x y z = any (==0) as || (map recip as == bs) where
  as = [ x,y,z, x+y, y+z, z+x, x+y+z ]
  bs = (unpackFlatArrayToList . batchInverse . packFlatArrayFromList) as

--------------------------------------------------------------------------------
