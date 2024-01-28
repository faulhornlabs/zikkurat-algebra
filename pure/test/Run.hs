
{-# LANGUAGE TypeApplications #-}
module Run where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.IORef

import System.IO.Unsafe
import System.Exit

import ZK.Algebra.Pure.Field.Class
--import ZK.Algebra.Pure.Curve.Class

import Field.Properties ( runRingTests  , runFieldTests )
-- import Curve.Properties ( runGroupTests , runCurveTests , runProjCurveTests )
-- import Poly.Properties  ( runPolyTests )

import qualified ZK.Algebra.Pure.Instances.BN254     as BN254
import qualified ZK.Algebra.Pure.Instances.BLS12_381 as BLS12_381
import qualified ZK.Algebra.Pure.Instances.BLS12_377 as BLS12_377

--------------------------------------------------------------------------------

printHeader :: String -> IO ()
printHeader str = do
  putStrLn ""
  putStrLn str
  putStrLn $ replicate (length str) '='

--------------------------------------------------------------------------------

{-# NOINLINE theOKFlag #-}
theOKFlag :: IORef Bool
theOKFlag = unsafePerformIO $ newIORef True

{-# NOINLINE testExit #-}
testExit :: IO ()
testExit = do
  b <- readIORef theOKFlag
  case b of
    True  -> return ()  -- exitWith  ExitSuccess
    False -> exitWith (ExitFailure 100)

--------------------------------------------------------------------------------

runTestsAll :: Int -> IO ()
runTestsAll n = do
  runTestsFields    n
  -- runTestsCurves     n
  -- runTestsPolys      n

  testExit

----------------------------------------

runTestsFields :: Int -> IO ()
runTestsFields n = do

  runTests_BN254     n
  runTests_BLS12_381 n
  runTests_BLS12_377 n

  testExit

----------------------------------------

runTests_BN254 :: Int -> IO ()
runTests_BN254 n = do

  printHeader "running tests for BN254/Fr"
  runFieldTests theOKFlag n (Proxy @BN254.Fr)

  printHeader "running tests for BN254/Fp"
  runFieldTests theOKFlag n (Proxy @BN254.Fp)

  printHeader "\nsanity check for BN254 extension tower"
  putStrLn $ "OK = " ++ show BN254.sanityCheckBN254

  printHeader "running tests for BN254/Fp2"
  runFieldTests theOKFlag n (Proxy @BN254.Fp2)

  printHeader "running tests for BN254/Fp6"
  runFieldTests theOKFlag n (Proxy @BN254.Fp6)

  printHeader "running tests for BN254/Fp12"
  runFieldTests theOKFlag n (Proxy @BN254.Fp12)

  testExit

----------------------------------------

runTests_BLS12_381 :: Int -> IO ()
runTests_BLS12_381 n = do

  printHeader "running tests for BLS12_381/Fr"
  runFieldTests theOKFlag n (Proxy @BLS12_381.Fr)

  printHeader "running tests for BLS12_381/Fp"
  runFieldTests theOKFlag n (Proxy @BLS12_381.Fp)

  printHeader "\nsanity check for BLS12_381 extension tower"
  putStrLn $ "OK = " ++ show BLS12_381.sanityCheckBLS12_381

  printHeader "running tests for BLS12_381/Fp2"
  runFieldTests theOKFlag n (Proxy @BLS12_381.Fp2)

  printHeader "running tests for BLS12_381/Fp6"
  runFieldTests theOKFlag n (Proxy @BLS12_381.Fp6)

  printHeader "running tests for BLS12_381/Fp12"
  runFieldTests theOKFlag n (Proxy @BLS12_381.Fp12)

  testExit

----------------------------------------

runTests_BLS12_377 :: Int -> IO ()
runTests_BLS12_377 n = do

  printHeader "running tests for BLS12_377/Fr"
  runFieldTests theOKFlag n (Proxy @BLS12_377.Fr)

  printHeader "running tests for BLS12_377/Fp"
  runFieldTests theOKFlag n (Proxy @BLS12_377.Fp)

  printHeader "running tests for BLS12_377/Fp2"
  runFieldTests theOKFlag n (Proxy @BLS12_377.Fp2)

  printHeader "running tests for BLS12_377/Fp6"
  runFieldTests theOKFlag n (Proxy @BLS12_377.Fp6)

  printHeader "running tests for BLS12_377/Fp12"
  runFieldTests theOKFlag n (Proxy @BLS12_377.Fp12)

  testExit
  
----------------------------------------

