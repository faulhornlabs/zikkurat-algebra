
-- | Property tests for elliptic curve groups

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Curve.Properties where

--------------------------------------------------------------------------------

import Data.Proxy

import Control.Monad

import System.Random
import System.IO

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve

--------------------------------------------------------------------------------

runCurveTests :: forall a. Curve a => Int -> Proxy a -> IO ()
runCurveTests n pxy = do
  runGroupTests     n pxy
  runCurveOnlyTests n pxy 

--------------------------------------------------------------------------------

runGroupTests :: forall a. Group a => Int -> Proxy a -> IO ()
runGroupTests n pxy = do

  forM_ groupProps $ \prop -> case prop of
  
    GroupProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    GroupProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    GroupProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

    GroupPropI1 test name -> doTests n name $ do
      k <- randomRIO (-1000,1000)
      x <- rndIO @a
      return (test k x) 

    GroupPropI2 test name -> doTests n name $ do
      k <- randomRIO (-1000,1000)
      x <- rndIO @a
      y <- rndIO @a
      return (test k x y) 

    GroupPropII1 test name -> doTests n name $ do
      k <- randomRIO (-1000,1000)
      l <- randomRIO (-1000,1000)
      x <- rndIO @a
      return (test k l x) 

runCurveOnlyTests :: forall a. Curve a => Int -> Proxy a -> IO ()
runCurveOnlyTests n pxy = do

  forM_ curveOnlyProps $ \prop -> case prop of
  
    CurveProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    CurveProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    CurveProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

    CurvePropI1 test name -> doTests n name $ do
      k <- randomRIO (-1000,1000)
      x <- rndIO @a
      return (test k x) 

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

data GroupProp
  = GroupProp1   (forall a. Group a  =>        a -> Bool          ) String
  | GroupProp2   (forall a. Group a  =>        a -> a -> Bool     ) String
  | GroupProp3   (forall a. Group a  =>        a -> a -> a -> Bool) String
  | GroupPropI1  (forall a. Group a  => Int -> a -> Bool          ) String
  | GroupPropI2  (forall a. Group a  => Int -> a -> a -> Bool     ) String
  | GroupPropII1 (forall a. Group a  => Int -> Int -> a -> Bool   ) String

data CurveProp
  = CurveProp1  (forall a. Curve a  => a -> Bool          ) String
  | CurveProp2  (forall a. Curve a  => a -> a -> Bool     ) String
  | CurveProp3  (forall a. Curve a  => a -> a -> a -> Bool) String
  | CurvePropI1 (forall a. Curve a  => Int -> a -> Bool   ) String

--------------------------------------------------------------------------------

groupProps :: [GroupProp]
groupProps = 
  [ GroupProp1 prop_add_left_unit               "add left unit"
  , GroupProp1 prop_add_right_unit              "add right unit"
  , GroupProp1 prop_add_left_inv                "add left inv"
  , GroupProp1 prop_add_right_inv               "add right inv"
  , GroupProp2 prop_add_commutative             "add comm"
  , GroupProp3 prop_add_associative             "add assoc"
  , GroupProp2 prop_sub_def                     "sub def"
  , GroupProp3 prop_add_sub_associative_1       "add-sub assoc /1"
  , GroupProp3 prop_add_sub_associative_2       "add-sub assoc /2"
  , GroupProp3 prop_add_sub_associative_3       "add-sub assoc /3"
  , GroupProp1 prop_normalize_invariant         "normalize"
  , GroupProp1 prop_is_zero                     "is zero"
  , GroupProp1 prop_is_equal                    "is equal"
  , GroupProp1 prop_dbl                         "dbl def"
  , GroupProp2 prop_dbl_distributive_1          "dbl distributive /1"
  , GroupProp2 prop_dbl_distributive_2          "dbl distributive /2"
  , GroupProp1 prop_neg_neg                     "neg . neg == id"
  , GroupProp1 prop_neg_dbl                     "neg (dbl x)"
  , GroupProp2 prop_neg_add                     "neg (add x y)"
    --
  , GroupProp1 prop_scale_1                     "scale 1" 
  , GroupProp1 prop_scale_2                     "scale 2" 
  , GroupProp1 prop_scale_3                     "scale 3" 
  , GroupProp1 prop_scale_4                     "scale 4" 
  , GroupProp1 prop_scale_5                     "scale 5" 
  , GroupProp1 prop_scale_zero                  "scale 0" 
  , GroupProp1 prop_scale_minus1                "scale -1" 
  , GroupProp1 prop_scale_minus2                "scale -2" 
  , GroupProp1 prop_scale_minus3                "scale -3" 
  , GroupPropI1  prop_scale_int_vs_integer      "scale vs. scale_"
  , GroupPropI1  prop_scale_plus                "scale (k+1)"
  , GroupPropI1  prop_scale_double_1            "scale (2*k) / 1"
  , GroupPropI1  prop_scale_double_2            "scale (2*k) / 2"
  , GroupPropII1 prop_scale_left_distrib        "scale (k+l) x"
  , GroupPropI2  prop_scale_right_distrib       "scale k (x+y)"
  ]

curveOnlyProps :: [CurveProp]
curveOnlyProps = 
  [ CurveProp1  prop_is_on_curve               "pt on curve"
  , CurveProp1  prop_is_on_curve_neg           "neg on curve"
  , CurveProp1  prop_is_on_curve_dbl           "dbl on curve"
  , CurveProp2  prop_is_on_curve_add           "add on curve"
  , CurveProp1  prop_is_on_curve_add_to_self   "x+x on curve"
  , CurveProp2  prop_is_on_curve_sub           "sub on curve"
  , CurvePropI1 prop_is_on_curve_scale         "scale on curve"
  , CurveProp1  prop_normalize_on_curve        "normalize on curve"
  ]

--------------------------------------------------------------------------------
-- * Group properties

prop_add_left_unit :: Group a => a -> Bool
prop_add_left_unit x = grpUnit `grpAdd` x == x

prop_add_right_unit :: Group a => a -> Bool
prop_add_right_unit x = x `grpAdd` grpUnit == x

prop_add_left_inv :: Group a => a -> Bool
prop_add_left_inv x = (grpNeg x) `grpAdd` x == grpUnit

prop_add_right_inv :: Group a => a -> Bool
prop_add_right_inv x = x `grpAdd` (grpNeg x) == grpUnit

prop_add_commutative :: Group a => a -> a -> Bool
prop_add_commutative x y = (x `grpAdd` y == y `grpAdd` x)

prop_add_associative :: Group a => a -> a -> a -> Bool
prop_add_associative x y z = ((x `grpAdd` y) `grpAdd` z) == (x `grpAdd` (y `grpAdd` z))

prop_sub_def :: Group a => a -> a -> Bool
prop_sub_def x y = (x `grpAdd` (grpNeg y) == x `grpSub` y)

prop_add_sub_associative_1 :: Group a => a -> a -> a -> Bool
prop_add_sub_associative_1 x y z = ((x `grpAdd` y) `grpSub` z) == (x `grpAdd` (y `grpSub` z))

prop_add_sub_associative_2 :: Group a => a -> a -> a -> Bool
prop_add_sub_associative_2 x y z = ((x `grpSub` y) `grpAdd` z) == (x `grpSub` (y `grpSub` z))

prop_add_sub_associative_3 :: Group a => a -> a -> a -> Bool
prop_add_sub_associative_3 x y z = ((x `grpSub` y) `grpSub` z) == (x `grpSub` (y `grpAdd` z))

prop_neg_neg :: Group a => a -> Bool
prop_neg_neg x = grpNeg (grpNeg x) == x

prop_neg_dbl :: Group a => a -> Bool
prop_neg_dbl x = grpNeg (grpDbl x) == grpDbl (grpNeg x)

prop_neg_add :: Group a => a -> a -> Bool
prop_neg_add x y = grpNeg (grpAdd x y) == (grpNeg x) `grpAdd` (grpNeg y)

----------------------------------------

prop_dbl :: Group a => a -> Bool
prop_dbl x = grpAdd x x == grpDbl x

prop_dbl_distributive_1 :: Group a => a -> a -> Bool
prop_dbl_distributive_1 x y = (grpDbl (x `grpAdd` y)) == ((grpDbl x) `grpAdd` (grpDbl y))

prop_dbl_distributive_2 :: Group a => a -> a -> Bool
prop_dbl_distributive_2 x y = (grpDbl (x `grpSub` y)) == ((grpDbl x) `grpSub` (grpDbl y))

prop_scale_1 :: Group a => a -> Bool
prop_scale_1 x  = (grpScale  1 x == x) 
               && (grpScale_ 1 x == x)

prop_scale_2 :: Group a => a -> Bool
prop_scale_2 x  = (grpScale  2 x == grpDbl x) 
               && (grpScale_ 2 x == grpDbl x)

prop_scale_3 :: Group a => a -> Bool
prop_scale_3 x  = (grpScale  3 x == grpAdd x (grpDbl x))
               && (grpScale_ 3 x == grpAdd x (grpDbl x))

prop_scale_4 :: Group a => a -> Bool
prop_scale_4 x  = (grpScale  4 x == grpDbl (grpDbl x))
               && (grpScale_ 4 x == grpDbl (grpDbl x))

prop_scale_5 :: Group a => a -> Bool
prop_scale_5 x  = (grpScale  5 x == grpAdd x (grpDbl (grpDbl x)))
               && (grpScale_ 5 x == grpAdd x (grpDbl (grpDbl x)))

prop_scale_zero :: Group a => a -> Bool
prop_scale_zero x  = (grpScale  0 x == grpUnit) 
                  && (grpScale_ 0 x == grpUnit)

prop_scale_minus1 :: Group a => a -> Bool
prop_scale_minus1 x  = (grpScale  (-1) x == grpNeg x) 
                    && (grpScale_ (-1) x == grpNeg x)

prop_scale_minus2 :: Group a => a -> Bool
prop_scale_minus2 x  = (grpScale  (-2) x == grpNeg (grpDbl x)) 
                    && (grpScale_ (-2) x == grpNeg (grpDbl x))

prop_scale_minus3 :: Group a => a -> Bool
prop_scale_minus3 x  = (grpScale  (-3) x == grpNeg (grpAdd x (grpDbl x)))
                    && (grpScale_ (-3) x == grpNeg (grpAdd x (grpDbl x)))

prop_scale_int_vs_integer :: Group a => Int -> a -> Bool
prop_scale_int_vs_integer k x = grpScale_ k x == grpScale (fromIntegral k) x

prop_scale_plus :: Group a => Int -> a -> Bool
prop_scale_plus k x = grpScale_ (k+1) x == grpAdd x (grpScale_ k x)

prop_scale_double_1 :: Group a => Int -> a -> Bool
prop_scale_double_1 k x = grpScale_ (2*k) x == grpDbl (grpScale_ k x)

prop_scale_double_2 :: Group a => Int -> a -> Bool
prop_scale_double_2 k x = grpScale_ (2*k) x == grpScale_ k (grpDbl x)

prop_scale_left_distrib :: Group a => Int -> Int -> a -> Bool 
prop_scale_left_distrib k l x = grpScale_ (k+l) x == grpAdd (grpScale_ k x) (grpScale_ l x)

prop_scale_right_distrib :: Group a => Int -> a -> a -> Bool 
prop_scale_right_distrib k x y = grpScale_ k (grpAdd x y) == grpAdd (grpScale_ k x) (grpScale_ k y)

----------------------------------------

prop_normalize_on_curve :: forall a. Curve a => a -> Bool
prop_normalize_on_curve x = isOnCurve (grpNormalize x)

prop_normalize_invariant :: forall a. Group a => a -> Bool
prop_normalize_invariant x = (x == grpNormalize x)

-- prop_normalize_is_normal :: forall a. ProjCurve a => a -> Bool
-- prop_normalize_is_normal x = let y = grpNormalize x in
--   grpIsUnit x || 

prop_is_zero :: forall a. Group a => a -> Bool
prop_is_zero x = grpIsUnit (grpUnit @a) && grpIsUnit x == (x == grpUnit)

prop_is_equal :: forall a. Group a => a -> Bool
prop_is_equal x = and
  [ grpUnit  == grpUnit @a
  , x        == x
  , grpIsUnit x || (x /= grpUnit) 
  ]

--------------------------------------------------------------------------------
-- * curve properties

prop_is_on_curve :: Curve a => a -> Bool
prop_is_on_curve x = isOnCurve x

prop_is_on_curve_neg :: Curve a => a -> Bool
prop_is_on_curve_neg x = isOnCurve (grpNeg x)

prop_is_on_curve_dbl :: Curve a => a -> Bool
prop_is_on_curve_dbl x = isOnCurve (grpDbl x)

prop_is_on_curve_add :: Curve a => a -> a -> Bool
prop_is_on_curve_add x y = isOnCurve (grpAdd x y)

prop_is_on_curve_add_to_self :: Curve a => a -> Bool
prop_is_on_curve_add_to_self x = isOnCurve (grpAdd x x)

prop_is_on_curve_sub :: Curve a => a -> a -> Bool
prop_is_on_curve_sub x y = isOnCurve (grpSub x y)

prop_is_on_curve_scale :: Curve a => Int -> a -> Bool
prop_is_on_curve_scale k x = isOnCurve (grpScale_ k x)

--------------------------------------------------------------------------------
