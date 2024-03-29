
-- | Property tests for elliptic curve pairings

{-# LANGUAGE ScopedTypeVariables, Rank2Types, TypeApplications #-}
module ZK.Test.Curve.Pairings where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Proxy

import Control.Monad

import System.Random
import System.IO

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.Misc

import qualified ZK.Algebra.Curves.BN128.G1.Affine       as BN128
import qualified ZK.Algebra.Curves.BN128.G2.Affine       as BN128

import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine   as BLS12_381
import qualified ZK.Algebra.Curves.BLS12_381.G2.Affine   as BLS12_381

import qualified ZK.Algebra.Curves.BN128.Pairing         as Fast.BN128
import qualified ZK.Algebra.Curves.BLS12_381.Pairing     as Fast.BLS12_381

import qualified ZK.Algebra.Reference.Pairing.BN128      as Ref.BN128
import qualified ZK.Algebra.Reference.Pairing.BLS12_381  as Ref.BLS12_381

--------------------------------------------------------------------------------

runTestsPairing_BN128 :: Int -> IO ()
runTestsPairing_BN128 n = do
  let n' = min n 25
  runPairingTests n' (Proxy @BN128.G1) (Proxy @BN128.G2) pairingProps_BN128

runTestsPairing_BLS12_381 :: Int -> IO ()
runTestsPairing_BLS12_381 n = do
  let n' = min n 25
  runPairingTests n' (Proxy @BLS12_381.G1) (Proxy @BLS12_381.G2) pairingProps_BLS12_381

--------------------------------------------------------------------------------

runPairingTests :: forall a b. (Curve a, Curve b) => Int -> Proxy a -> Proxy b -> [PairingProp a b] -> IO ()
runPairingTests n pxy1 pxy2 properties = do

  forM_ properties $ \prop -> case prop of

    PairingProp12 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @b
      return (test x y) 
  
    PairingProp112 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @a
      z <- rndIO @b
      return (test x y z) 

    PairingProp122 test name -> doTests n name $ do
      x <- rndIO @a
      y <- rndIO @b
      z <- rndIO @b
      return (test x y z) 

    PairingPropI12 test name -> doTests n name $ do
      k <- randomRIO (-10000,10000)
      x <- rndIO @a
      y <- rndIO @b
      return (test k x y) 

    PairingPropII test name -> doTests n name $ do
      k <- randomRIO (-10000,10000)
      l <- randomRIO (-10000,10000)
      return (test k l) 

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

data PairingProp a b
  = PairingProp12  (      a -> b       -> Bool) String
  | PairingProp112 (      a -> a -> b  -> Bool) String
  | PairingProp122 (      a -> b -> b  -> Bool) String
  | PairingPropI12 (Integer -> a -> b  -> Bool) String
  | PairingPropII  (Integer -> Integer -> Bool) String

--------------------------------------------------------------------------------
-- * properties

pairingProps_BN128 :: [PairingProp BN128.G1 BN128.G2]
pairingProps_BN128 = 
  [ PairingProp112  prop_ref_left_linear_bn128       "pairing left-linear"
  , PairingProp122  prop_ref_right_linear_bn128      "pairing right-linear"
  , PairingPropI12  prop_ref_scale_bn128             "left/right scale"
  , PairingPropI12  prop_ref_scale_l_bn128           "left scale"
  , PairingPropI12  prop_ref_scale_r_bn128           "right scale"
  , PairingProp12   prop_ref_left_inf_bn128          "<inf,b> = 1"
  , PairingProp12   prop_ref_right_inf_bn128         "<a,inf> = 1"
  , PairingPropII   prop_ref_nondegenerate_bn128     "non-degenerate"
  , PairingProp12   prop_ref_against_fast_bn128      "ref against fast"
  ]

pairingProps_BLS12_381 :: [PairingProp BLS12_381.G1 BLS12_381.G2]
pairingProps_BLS12_381 = 
  [ PairingProp112  prop_ref_left_linear_bls12_381   "pairing left-linear"
  , PairingProp122  prop_ref_right_linear_bls12_381  "pairing right-linear"
  , PairingPropI12  prop_ref_scale_bls12_381         "left/right scale"
  , PairingPropI12  prop_ref_scale_l_bls12_381       "left scale"
  , PairingPropI12  prop_ref_scale_r_bls12_381       "right scale"
  , PairingProp12   prop_ref_left_inf_bls12_381      "<inf,b> = 1"
  , PairingProp12   prop_ref_right_inf_bls12_381     "<a,inf> = 1"
  , PairingPropII   prop_ref_nondegenerate_bls12_381 "non-degenerate"
  , PairingProp12   prop_ref_against_fast_bls12_381  "ref against fast"
  ]

----------------------------------------

prop_ref_left_linear_bn128 :: BN128.G1 -> BN128.G1 -> BN128.G2 -> Bool
prop_ref_left_linear_bn128 a b c = (Ref.BN128.pairing a c) * (Ref.BN128.pairing b c) == Ref.BN128.pairing (a `grpAdd` b) c

prop_ref_right_linear_bn128 :: BN128.G1 -> BN128.G2 -> BN128.G2 -> Bool
prop_ref_right_linear_bn128 a b c = (Ref.BN128.pairing a b) * (Ref.BN128.pairing a c) == Ref.BN128.pairing a (b `grpAdd` c)

prop_ref_scale_bn128 :: Integer -> BN128.G1 -> BN128.G2 -> Bool
prop_ref_scale_bn128 k a b = Ref.BN128.pairing (grpScale k a) b == Ref.BN128.pairing a (grpScale k b)

prop_ref_scale_l_bn128 :: Integer -> BN128.G1 -> BN128.G2 -> Bool
prop_ref_scale_l_bn128 k a b = Ref.BN128.pairing (grpScale k a) b == (Ref.BN128.pairing a b) `power` k

prop_ref_scale_r_bn128 :: Integer -> BN128.G1 -> BN128.G2 -> Bool
prop_ref_scale_r_bn128 k a b = Ref.BN128.pairing a (grpScale k b) == (Ref.BN128.pairing a b) `power` k

prop_ref_left_inf_bn128 :: BN128.G1 -> BN128.G2 -> Bool
prop_ref_left_inf_bn128 a b = Ref.BN128.pairing grpUnit b == 1

prop_ref_right_inf_bn128 :: BN128.G1 -> BN128.G2 -> Bool
prop_ref_right_inf_bn128 a b = Ref.BN128.pairing a grpUnit == 1

prop_ref_nondegenerate_bn128 :: Integer -> Integer -> Bool
prop_ref_nondegenerate_bn128 k l = (k==0) || (l==0) || Ref.BN128.pairing (grpScale k BN128.genG1) (grpScale l BN128.genG2) /= 1

prop_ref_against_fast_bn128 :: BN128.G1 -> BN128.G2 -> Bool
prop_ref_against_fast_bn128 a b = Ref.BN128.pairing a b == Fast.BN128.pairing a b

----------------------------------------

prop_ref_left_linear_bls12_381 :: BLS12_381.G1 -> BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_left_linear_bls12_381 a b c = (Ref.BLS12_381.pairing a c) * (Ref.BLS12_381.pairing b c) == Ref.BLS12_381.pairing (a `grpAdd` b) c

prop_ref_right_linear_bls12_381 :: BLS12_381.G1 -> BLS12_381.G2 -> BLS12_381.G2 -> Bool
prop_ref_right_linear_bls12_381 a b c = (Ref.BLS12_381.pairing a b) * (Ref.BLS12_381.pairing a c) == Ref.BLS12_381.pairing a (b `grpAdd` c)

prop_ref_scale_bls12_381 :: Integer -> BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_scale_bls12_381 k a b = Ref.BLS12_381.pairing (grpScale k a) b == Ref.BLS12_381.pairing a (grpScale k b)

prop_ref_scale_l_bls12_381 :: Integer -> BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_scale_l_bls12_381 k a b = Ref.BLS12_381.pairing (grpScale k a) b == (Ref.BLS12_381.pairing a b) `power` k

prop_ref_scale_r_bls12_381 :: Integer -> BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_scale_r_bls12_381 k a b = Ref.BLS12_381.pairing a (grpScale k b) == (Ref.BLS12_381.pairing a b) `power` k

prop_ref_left_inf_bls12_381 :: BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_left_inf_bls12_381 a b = Ref.BLS12_381.pairing grpUnit b == 1

prop_ref_right_inf_bls12_381 :: BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_right_inf_bls12_381 a b = Ref.BLS12_381.pairing a grpUnit == 1

prop_ref_nondegenerate_bls12_381 :: Integer -> Integer -> Bool
prop_ref_nondegenerate_bls12_381 k l = (k==0) || (l==0) || Ref.BLS12_381.pairing (grpScale k BLS12_381.genG1) (grpScale l BLS12_381.genG2) /= 1

prop_ref_against_fast_bls12_381 :: BLS12_381.G1 -> BLS12_381.G2 -> Bool
prop_ref_against_fast_bls12_381 a b = Ref.BLS12_381.pairing a b == Fast.BLS12_381.pairing a b

--------------------------------------------------------------------------------

