
{-# LANGUAGE TypeApplications #-}
module ZK.Test.Run where

--------------------------------------------------------------------------------

import Data.Proxy

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve

import ZK.Test.Field.Properties ( runRingTests  , runFieldTests )
import ZK.Test.Curve.Properties ( runGroupTests , runCurveTests )

import qualified ZK.Algebra.BigInt.BigInt128 as BigInt128
import qualified ZK.Algebra.BigInt.BigInt192 as BigInt192
import qualified ZK.Algebra.BigInt.BigInt256 as BigInt256
import qualified ZK.Algebra.BigInt.BigInt320 as BigInt320
import qualified ZK.Algebra.BigInt.BigInt384 as BigInt384

import qualified ZK.Algebra.Curves.BN128.Fp.Std  as BN128_Fp_Std
import qualified ZK.Algebra.Curves.BN128.Fr.Std  as BN128_Fr_Std
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as BN128_Fp_Mont
import qualified ZK.Algebra.Curves.BN128.Fr.Mont as BN128_Fr_Mont

import qualified ZK.Algebra.Curves.BLS12_381.Fp.Std  as BLS12_381_Fp_Std
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std  as BLS12_381_Fr_Std
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont as BLS12_381_Fp_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont as BLS12_381_Fr_Mont

import qualified ZK.Algebra.Curves.BN128.G1.Proj     as BN128_G1_Proj
import qualified ZK.Algebra.Curves.BLS12_381.G1.Proj as BLS12_381_G1_Proj

--------------------------------------------------------------------------------

printHeader :: String -> IO ()
printHeader str = do
  putStrLn ""
  putStrLn str
  putStrLn $ replicate (length str) '='

--------------------------------------------------------------------------------

runTestsAll :: Int -> IO ()
runTestsAll n = do
  runTestsBigInt    n 
  runTestsStdField  n
  runTestsMontField n
  runTestsProjCurve n

----------------------------------------

runTestsProjCurve :: Int -> IO ()
runTestsProjCurve n = do

  printHeader "running tests for BLS12-381/G1/Proj"
  runCurveTests n (Proxy @BLS12_381_G1_Proj.G1)

  printHeader "running tests for BN128/G1/Proj"
  runCurveTests n (Proxy @BN128_G1_Proj.G1)

----------------------------------------

runTestsBigInt :: Int -> IO ()
runTestsBigInt n = do

  printHeader "running tests for BigInt128"
  runRingTests n (Proxy @BigInt128.BigInt128)

  printHeader "running tests for BigInt192"
  runRingTests n (Proxy @BigInt192.BigInt192)

  printHeader "running tests for BigInt256"
  runRingTests n (Proxy @BigInt256.BigInt256)

  printHeader "running tests for BigInt320"
  runRingTests n (Proxy @BigInt320.BigInt320)

  printHeader "running tests for BigInt384"
  runRingTests n (Proxy @BigInt384.BigInt384)

----------------------------------------

runTestsStdField :: Int -> IO ()
runTestsStdField n = do

  printHeader "running tests for BN128/Fp/Std"
  runFieldTests n (Proxy @BN128_Fp_Std.Fp)

  printHeader "running tests for BN128/Fr/Std"
  runFieldTests n (Proxy @BN128_Fr_Std.Fr)
  
  printHeader "running tests for BLS12-381/Fp/Std"
  runFieldTests n (Proxy @BLS12_381_Fp_Std.Fp)

  printHeader "running tests for BLS12-381/Fr/Std"
  runFieldTests n (Proxy @BLS12_381_Fr_Std.Fr)

----------------------------------------

runTestsMontField :: Int -> IO ()
runTestsMontField n = do

  printHeader "running tests for BN128/Fp/Montgomery"
  runFieldTests n (Proxy @BN128_Fp_Mont.Fp)

  printHeader "running tests for BN128/Fr/Montgomery"
  runFieldTests n (Proxy @BN128_Fr_Mont.Fr)

  printHeader "running tests for BLS12-381/Fp/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fp_Mont.Fp)

  printHeader "running tests for BLS12-381/Fr/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fr_Mont.Fr)

--------------------------------------------------------------------------------

