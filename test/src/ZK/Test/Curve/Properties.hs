
-- | Property tests for elliptic curve groups

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Curve.Properties where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy

import Control.Monad

import System.Random
import System.IO

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Misc

--------------------------------------------------------------------------------

runCurveTests :: forall a. Curve a => Int -> Proxy a -> IO ()
runCurveTests n pxy = do
  runGroupTests     n pxy
  runCurveOnlyTests n pxy 

runProjCurveTests :: forall a. ProjCurve a => Int -> Proxy a -> IO ()
runProjCurveTests n pxy = do
  runGroupTests         n pxy
  runCurveOnlyTests     n pxy 
  runProjCurveOnlyTests n pxy 

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

runProjCurveOnlyTests :: forall a. ProjCurve a => Int -> Proxy a -> IO ()
runProjCurveOnlyTests n pxy = do

  forM_ projCurveOnlyProps $ \prop -> case prop of
  
    ProjCurveProp1 test name -> doTests n name $ do
      x <- rndIO @a
      return (test x) 

    ProjCurvePropA test name -> doTests n name $ do
      x <- rndIO @(AffinePoint a)
      return (test pxy x) 

    ProjCurveProp2 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      return (test x y) 

    ProjCurvePropI1 test name -> doTests n name $ do
      k <- randomRIO (-1000,1000)
      x <- rndIO @a
      return (test k x) 

    ProjCurveProp3 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @a
      return (test x y z) 

    ProjCurveProp3A test name -> doTests n name $ do
      x <- rndIO @(AffinePoint a)
      y <- rndIO @(AffinePoint a)
      z <- rndIO @(AffinePoint a)
      return (test pxy x y z) 

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
  = CurveProp1    (forall a. Curve a  => a -> Bool          ) String
  | CurveProp2    (forall a. Curve a  => a -> a -> Bool     ) String
  | CurveProp3    (forall a. Curve a  => a -> a -> a -> Bool) String
  | CurvePropI1   (forall a. Curve a  => Int -> a -> Bool   ) String

data ProjCurveProp
  = ProjCurveProp1  (forall a. ProjCurve a  => a -> Bool            ) String
  | ProjCurveProp2  (forall a. ProjCurve a  => a -> a -> Bool       ) String
  | ProjCurvePropA  (forall a. ProjCurve a  => Proxy a -> AffinePoint a -> Bool) String
  | ProjCurvePropI1 (forall a. ProjCurve a  => Int -> a -> Bool     ) String
  | ProjCurveProp3  (forall a. ProjCurve a  => a -> a -> a -> Bool  ) String
  | ProjCurveProp3A (forall a. ProjCurve a  => Proxy a -> AffinePoint a -> AffinePoint a -> AffinePoint a -> Bool) String

--------------------------------------------------------------------------------

referenceScale :: Group a => Integer -> a -> a
referenceScale k x0 
  | k == 0     = grpUnit
  | k < 0      = grpNeg $ go (-k) grpUnit x0
  | otherwise  =          go   k  grpUnit x0
  where
    go 0 acc _   = acc
    go k acc run = case (k .&. 1) of 
      0 -> go (shiftR k 1)         acc      (grpDbl run)
      _ -> go (shiftR k 1) (grpAdd acc run) (grpDbl run)

--------------------------------------------------------------------------------

groupProps :: [GroupProp]
groupProps = 
  [ GroupProp1 prop_add_left_unit               "add left unit"
  , GroupProp1 prop_add_right_unit              "add right unit"
  , GroupProp1 prop_neg_unit                    "neg unit"
  , GroupProp1 prop_sub_left_unit               "sub left unit"
  , GroupProp1 prop_sub_right_unit              "sub right unit"
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
  , GroupPropI1 prop_scale_vs_ref               "scale vs. reference"
  , GroupProp1  prop_scale_1                    "scale 1" 
  , GroupProp1  prop_scale_2                    "scale 2" 
  , GroupProp1  prop_scale_3                    "scale 3" 
  , GroupProp1  prop_scale_4                    "scale 4" 
  , GroupProp1  prop_scale_5                    "scale 5" 
  , GroupProp1  prop_scale_zero                 "scale 0" 
  , GroupProp1  prop_scale_minus1               "scale -1" 
  , GroupProp1  prop_scale_minus2               "scale -2" 
  , GroupProp1  prop_scale_minus3               "scale -3" 
  , GroupPropI1  prop_scale_int_vs_integer      "scale vs. scale_"
  , GroupPropI1  prop_scale_plus                "scale (k+1)"
  , GroupPropI1  prop_scale_double_1            "scale (2*k) / 1"
  , GroupPropI1  prop_scale_double_2            "scale (2*k) / 2"
  , GroupPropII1 prop_scale_left_distrib        "scale (k+l) x"
  , GroupPropI2  prop_scale_right_distrib       "scale k (x+y)"
  ]

--------------------------------------------------------------------------------
-- * Group properties

prop_add_left_unit :: Group a => a -> Bool
prop_add_left_unit x = grpUnit `grpAdd` x == x

prop_add_right_unit :: Group a => a -> Bool
prop_add_right_unit x = x `grpAdd` grpUnit == x

prop_sub_left_unit :: Group a => a -> Bool
prop_sub_left_unit x = grpUnit `grpSub` x == grpNeg x

prop_sub_right_unit :: Group a => a -> Bool
prop_sub_right_unit x = x `grpSub` grpUnit == x

prop_neg_unit :: forall a. Group a => a -> Bool
prop_neg_unit _ = grpNeg (grpUnit :: a) == grpUnit

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

prop_scale_vs_ref :: Group a => Int -> a -> Bool
prop_scale_vs_ref k x =
  (grpScale (fromIntegral k) x == ref) && (grpScale_ k x == ref) where
    ref = referenceScale (fromIntegral k) x

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
-- * projective properties

projCurveOnlyProps :: [ProjCurveProp]
projCurveOnlyProps = 
  [ ProjCurveProp1   prop_to_from_affine            "fromAffine . toAffine"  
  , ProjCurvePropA   prop_from_to_affine            "toAffine . fromAffine"  
  , ProjCurveProp3   prop_batch_to_from_affine      "batch from/to affine"
  , ProjCurveProp3A  prop_batch_from_to_affine      "batch to/from affine"
  , ProjCurveProp1   prop_is_on_curve_toAffine      "oncurve(toAffine(x))"  
  , ProjCurveProp1   prop_normalize_then_toAffine   "toAffine(normalize(x))"  
  , ProjCurvePropA   prop_is_on_curve_fromAffine    "oncurve(fromAffine(x))"  
  , ProjCurveProp2   prop_mixed_add                 "mixed addition" 
  , ProjCurveProp1   prop_mixed_add_dbl             "mixed add with itself" 
  , ProjCurveProp1   prop_mixed_add_left_unit       "mixed add left unit" 
  , ProjCurveProp1   prop_mixed_add_right_unit      "mixed add right unit" 
  , ProjCurveProp1   prop_mixed_add_left_inv        "mixed add left inv" 
  , ProjCurveProp1   prop_mixed_add_right_inv       "mixed add right inv" 
  , ProjCurveProp2   prop_to_proj_vs_affine_add     "add proj vs. affine" 
  , ProjCurveProp2   prop_to_proj_vs_affine_sub     "sub proj vs. affine" 
  , ProjCurveProp1   prop_to_proj_vs_affine_dbl     "dbl proj vs. affine" 
  , ProjCurveProp1   prop_to_proj_vs_affine_neg     "neg proj vs. affine" 
  , ProjCurvePropI1  prop_to_proj_vs_affine_scl     "scl proj vs. affine" 
  ]

prop_to_from_affine :: ProjCurve a => a -> Bool
prop_to_from_affine x = (fromAffine (toAffine x)) == x

prop_from_to_affine :: forall a. ProjCurve a => Proxy a -> AffinePoint a -> Bool
prop_from_to_affine _ x = toAffine (fromAffine @a x) == x

prop_batch_to_from_affine :: forall a. ProjCurve a => a -> a -> a -> Bool
prop_batch_to_from_affine x y z = lhs == rhs where
  lhs = [x, y, z, grpAdd x y, grpAdd x z, grpAdd y z] :: [a]
  inp = packFlatArrayFromList lhs                     :: FlatArray a
  out = batchFromAffine @a (batchToAffine @a inp)     :: FlatArray a
  rhs = unpackFlatArrayToList out                     :: [a]

prop_batch_from_to_affine :: forall a. ProjCurve a => Proxy a -> AffinePoint a -> AffinePoint a -> AffinePoint a -> Bool
prop_batch_from_to_affine _ x y z = lhs == rhs where
  lhs = [x, y, z, grpAdd x y, grpAdd x z, grpAdd y z] :: [AffinePoint a]
  inp = packFlatArrayFromList lhs                     :: FlatArray (AffinePoint a) 
  out = batchToAffine @a (batchFromAffine @a inp)     :: FlatArray (AffinePoint a)
  rhs = unpackFlatArrayToList out                     :: [AffinePoint a]

prop_is_on_curve_toAffine :: ProjCurve a => a -> Bool
prop_is_on_curve_toAffine x = isOnCurve (toAffine x)

prop_normalize_then_toAffine :: ProjCurve a => a -> Bool
prop_normalize_then_toAffine x = toAffine x == toAffine (grpNormalize x)

prop_is_on_curve_fromAffine :: forall a. ProjCurve a => Proxy a -> AffinePoint a -> Bool
prop_is_on_curve_fromAffine _ x = isOnCurve (fromAffine @a x)

prop_mixed_add :: ProjCurve a => a -> a -> Bool
prop_mixed_add x y = mixedAdd x (toAffine y) == grpAdd x y

prop_mixed_add_dbl :: ProjCurve a => a -> Bool
prop_mixed_add_dbl x = mixedAdd x (toAffine x) == grpDbl x

prop_mixed_add_left_unit :: ProjCurve a => a -> Bool
prop_mixed_add_left_unit x = mixedAdd infinity (toAffine x) == x

prop_mixed_add_right_unit :: ProjCurve a => a -> Bool
prop_mixed_add_right_unit x = mixedAdd x infinity == x

prop_mixed_add_left_inv :: ProjCurve a => a -> Bool
prop_mixed_add_left_inv x = mixedAdd (grpNeg x) (toAffine x) == infinity

prop_mixed_add_right_inv :: ProjCurve a => a -> Bool
prop_mixed_add_right_inv x = mixedAdd x (toAffine $ grpNeg x) == infinity

prop_to_proj_vs_affine_add :: ProjCurve a => a -> a -> Bool
prop_to_proj_vs_affine_add x y = toAffine (grpAdd x y) == grpAdd (toAffine x) (toAffine y)

prop_to_proj_vs_affine_sub :: ProjCurve a => a -> a -> Bool
prop_to_proj_vs_affine_sub x y = toAffine (grpSub x y) == grpSub (toAffine x) (toAffine y)

prop_to_proj_vs_affine_dbl :: ProjCurve a => a -> Bool
prop_to_proj_vs_affine_dbl x = toAffine (grpDbl x) == grpDbl (toAffine x)

prop_to_proj_vs_affine_neg :: ProjCurve a => a -> Bool
prop_to_proj_vs_affine_neg x = toAffine (grpNeg x) == grpNeg (toAffine x)

prop_to_proj_vs_affine_scl :: ProjCurve a => Int -> a -> Bool
prop_to_proj_vs_affine_scl k x = toAffine (grpScale_ k x) == grpScale_ k (toAffine x)

--------------------------------------------------------------------------------

