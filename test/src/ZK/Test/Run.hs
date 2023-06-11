
{-# LANGUAGE TypeApplications #-}
module ZK.Test.Run where

--------------------------------------------------------------------------------

import Data.Proxy

import ZK.Algebra.Class.Field
import ZK.Test.Field.Properties

import qualified ZK.Algebra.BigInt.BigInt128 as BigInt128
import qualified ZK.Algebra.BigInt.BigInt192 as BigInt192
import qualified ZK.Algebra.BigInt.BigInt256 as BigInt256
import qualified ZK.Algebra.BigInt.BigInt320 as BigInt320

import qualified ZK.Algebra.Curves.BN128.Std.Fp  as BN128_Fp_Std
import qualified ZK.Algebra.Curves.BN128.Std.Fr  as BN128_Fr_Std
import qualified ZK.Algebra.Curves.BN128.Mont.Fp as BN128_Fp_Mont
import qualified ZK.Algebra.Curves.BN128.Mont.Fr as BN128_Fr_Mont

import qualified ZK.Algebra.Curves.BLS12_381.Std.Fp  as BLS12_381_Fp_Std
import qualified ZK.Algebra.Curves.BLS12_381.Std.Fr  as BLS12_381_Fr_Std
import qualified ZK.Algebra.Curves.BLS12_381.Mont.Fp as BLS12_381_Fp_Mont
import qualified ZK.Algebra.Curves.BLS12_381.Mont.Fr as BLS12_381_Fr_Mont

--------------------------------------------------------------------------------

printHeader :: String -> IO ()
printHeader str = do
  putStrLn ""
  putStrLn str
  putStrLn $ replicate (length str) '='

--------------------------------------------------------------------------------


runTests :: Int -> IO ()
runTests n = do

  printHeader "running tests for BigInt128"
  runRingTests n (Proxy @BigInt128.BigInt128)

  printHeader "running tests for BigInt192"
  runRingTests n (Proxy @BigInt192.BigInt192)

  printHeader "running tests for BigInt256"
  runRingTests n (Proxy @BigInt256.BigInt256)

  printHeader "running tests for BigInt320"
  runRingTests n (Proxy @BigInt320.BigInt320)

  -----------

  printHeader "running tests for BN128/Fp/Std"
  runFieldTests n (Proxy @BN128_Fp_Std.Fp)

  printHeader "running tests for BN128/Fr/Std"
  runFieldTests n (Proxy @BN128_Fr_Std.Fr)

  printHeader "running tests for BN128/Fp/Montgomery"
  runFieldTests n (Proxy @BN128_Fp_Mont.Fp)

  printHeader "running tests for BN128/Fr/Montgomery"
  runFieldTests n (Proxy @BN128_Fr_Mont.Fr)

  -----------
  
  printHeader "running tests for BLS12-381/Fp/Std"
  runFieldTests n (Proxy @BLS12_381_Fp_Std.Fp)

  printHeader "running tests for BLS12-381/Fr/Std"
  runFieldTests n (Proxy @BLS12_381_Fr_Std.Fr)

  printHeader "running tests for BLS12-381/Fp/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fp_Mont.Fp)

  printHeader "running tests for BLS12-381/Fr/Montgomery"
  runFieldTests n (Proxy @BLS12_381_Fr_Mont.Fr)

--------------------------------------------------------------------------------

