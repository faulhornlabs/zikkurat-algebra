
{-# LANGUAGE TypeApplications #-}
module ZK.Test.Run where

--------------------------------------------------------------------------------

import Data.Proxy

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve

import ZK.Test.Platform.Properties  ( runPlatformTests )
import ZK.Test.Field.Properties ( runRingTests  , runFieldTests )
import ZK.Test.Curve.Properties ( runGroupTests , runCurveTests , runProjCurveTests )
import ZK.Test.Poly.Properties  ( runPolyTests )
import ZK.Test.Field.Ref_BN254     ( runTests_compare_BN254     )
import ZK.Test.Field.Ref_BLS12_381 ( runTests_compare_BLS12_381 )

import qualified ZK.Algebra.BigInt.Platform            as Platform

import qualified ZK.Algebra.BigInt.BigInt128           as BigInt128
import qualified ZK.Algebra.BigInt.BigInt192           as BigInt192
import qualified ZK.Algebra.BigInt.BigInt256           as BigInt256
import qualified ZK.Algebra.BigInt.BigInt320           as BigInt320
import qualified ZK.Algebra.BigInt.BigInt384           as BigInt384

import qualified ZK.Algebra.Curves.BN128.Fr.Std        as BN128_Fr_Std
import qualified ZK.Algebra.Curves.BN128.Fp.Std        as BN128_Fp_Std
import qualified ZK.Algebra.Curves.BN128.Fr.Mont       as BN128_Fr_Mont
import qualified ZK.Algebra.Curves.BN128.Fp.Mont       as BN128_Fp_Mont
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont      as BN128_Fp2_Mont
import qualified ZK.Algebra.Curves.BN128.Fp6.Mont      as BN128_Fp6_Mont
import qualified ZK.Algebra.Curves.BN128.Fp12.Mont     as BN128_Fp12_Mont

import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std    as BLS12_381_Fr_Std
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Std    as BLS12_381_Fp_Std
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont   as BLS12_381_Fr_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont   as BLS12_381_Fp_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont  as BLS12_381_Fp2_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp6.Mont  as BLS12_381_Fp6_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp12.Mont as BLS12_381_Fp12_Mont

import qualified ZK.Algebra.Curves.BN128.G1.Proj       as BN128_G1_Proj
import qualified ZK.Algebra.Curves.BN128.G1.Jac        as BN128_G1_Jac
import qualified ZK.Algebra.Curves.BN128.G1.Affine     as BN128_G1_Affine
import qualified ZK.Algebra.Curves.BN128.G2.Proj       as BN128_G2_Proj
import qualified ZK.Algebra.Curves.BN128.G2.Affine     as BN128_G2_Affine

import qualified ZK.Algebra.Curves.BLS12_381.G1.Proj   as BLS12_381_G1_Proj
import qualified ZK.Algebra.Curves.BLS12_381.G1.Jac    as BLS12_381_G1_Jac
import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine as BLS12_381_G1_Affine
import qualified ZK.Algebra.Curves.BLS12_381.G2.Proj   as BLS12_381_G2_Proj
import qualified ZK.Algebra.Curves.BLS12_381.G2.Affine as BLS12_381_G2_Affine

import qualified ZK.Algebra.Curves.BN128.Poly          as BN128_Poly
import qualified ZK.Algebra.Curves.BLS12_381.Poly      as BLS12_381_Poly

--------------------------------------------------------------------------------

printHeader :: String -> IO ()
printHeader str = do
  putStrLn ""
  putStrLn str
  putStrLn $ replicate (length str) '='

--------------------------------------------------------------------------------

runTestsAll :: Int -> IO ()
runTestsAll n = do
  runTestsPlatform   
  runTestsBigInt        n 
  runTestsStdField      n
  runTestsMontField     n
  runTestsProjCurve     n
  runTestsAffineCurve   n
  runTestsJacCurve      n
  runTestsProjCurveG2   n
  runTestsAffineCurveG2 n
  runTestsPolys         n
  runTestsCompare       n

----------------------------------------

runTestsPlatform :: IO ()
runTestsPlatform = do
  printHeader "running tests for platform-specific C/asm code"
  runPlatformTests

----------------------------------------

runTestsCompare :: Int -> IO ()
runTestsCompare n = do
  printHeader "running tests comparing against pure Haskell implementation"
  runTests_compare_BN254     n
  runTests_compare_BLS12_381 n 

----------------------------------------

runTestsPolys :: Int -> IO ()
runTestsPolys n = do

  printHeader "running tests for BLS12-381/Poly"
  runPolyTests n (Proxy @BLS12_381_Poly.Poly)

  printHeader "running tests for BN128/Poly"
  runPolyTests n (Proxy @BN128_Poly.Poly)

----------------------------------------

runTestsProjCurve :: Int -> IO ()
runTestsProjCurve n = do

  printHeader "running tests for BLS12-381/G1/Proj"
  runProjCurveTests n (Proxy @BLS12_381_G1_Proj.G1)

  printHeader "running tests for BN128/G1/Proj"
  runProjCurveTests n (Proxy @BN128_G1_Proj.G1)

runTestsProjCurveG2 :: Int -> IO ()
runTestsProjCurveG2 n = do

  printHeader "running tests for BLS12-381/G2/Proj"
  runProjCurveTests n (Proxy @BLS12_381_G2_Proj.G2)

  printHeader "running tests for BN128/G2/Proj"
  runProjCurveTests n (Proxy @BN128_G2_Proj.G2)

----------------------------------------

runTestsJacCurve :: Int -> IO ()
runTestsJacCurve n = do

  printHeader "running tests for BLS12-381/G1/Jac"
  runProjCurveTests n (Proxy @BLS12_381_G1_Jac.G1)

  printHeader "running tests for BN128/G1/Jac"
  runProjCurveTests n (Proxy @BN128_G1_Jac.G1)

----------------------------------------

runTestsAffineCurve :: Int -> IO ()
runTestsAffineCurve n = do

  printHeader "running tests for BLS12-381/G1/Affine"
  runCurveTests n (Proxy @BLS12_381_G1_Affine.G1)

  printHeader "running tests for BN128/G1/Affine"
  runCurveTests n (Proxy @BN128_G1_Affine.G1)

runTestsAffineCurveG2 :: Int -> IO ()
runTestsAffineCurveG2 n = do

  printHeader "running tests for BLS12-381/G2/Affine"
  runCurveTests n (Proxy @BLS12_381_G2_Affine.G2)

  printHeader "running tests for BN128/G2/Affine"
  runCurveTests n (Proxy @BN128_G2_Affine.G2)

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

runTestsFieldTowers :: Int -> IO ()
runTestsFieldTowers n = do

  printHeader "running tests for BN128/Fp2/Montgomery"
  runFieldTests n (Proxy @BN128_Fp2_Mont.Fp2)

  printHeader "running tests for BN128/Fp6/Montgomery"
  runFieldTests n (Proxy @BN128_Fp6_Mont.Fp6)

  printHeader "running tests for BN128/Fp12/Montgomery"
  runFieldTests n (Proxy @BN128_Fp12_Mont.Fp12)

  --

  printHeader "running tests for BLS12_381/Fp2/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fp2_Mont.Fp2)

  printHeader "running tests for BLS12_381/Fp6/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fp6_Mont.Fp6)

  printHeader "running tests for BLS12_381/Fp12/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fp12_Mont.Fp12)

--------------------------------------------------------------------------------
