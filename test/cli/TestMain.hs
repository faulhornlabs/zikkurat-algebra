
module Main where

--------------------------------------------------------------------------------

import Data.Char

import Text.Read

import System.Environment

import ZK.Test.Run

--------------------------------------------------------------------------------

defaultN :: Int
defaultN = 1000

--------------------------------------------------------------------------------

help :: IO ()
help = do
  putStrLn "zikkurat-algebra-tests"
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ zikkurat-algebra-tests [<nnn>]"
  putStrLn "$ zikkurat-algebra-tests <suite> [<nnn>]"
  putStrLn ""
  putStrLn "the following testsuites are available:"
  putStrLn ""
  putStrLn " - all"
  putStrLn " - platform"  
  putStrLn " - bigint"
  putStrLn " - std_field"
  putStrLn " - montgomery"
  putStrLn " - towers"
  putStrLn " - extfield"
  putStrLn " - againstref"
  putStrLn " - affine_curve"
  putStrLn " - proj_curve"
  putStrLn " - jac_curve"
  putStrLn " - affine_curve_g2"
  putStrLn " - proj_curve_g2"
  putStrLn " - poly"
  putStrLn ""

--------------------------------------------------------------------------------

validNames :: [String]
validNames = 
  [ "all"
  , "platform"
  , "bigint"
  , "stdfield"
  , "montfield" , "montgomery"
  , "towers" , "extfield"
  , "compare" , "againstref" , "reference"
  , "affcurve"   , "affinecurve"   , "affine"
  , "affcurveg2" , "affinecurveg2" , "affineg2"
  , "projcurve"   , "projective"   , "proj" 
  , "projcurveg2" , "projectiveg2" , "projg2"
  , "jaccurve" , "jacobiancurve" , "jacobian"
  , "poly" , "polynomial" , "univariate"
  ]

testMain :: String -> Int -> IO ()
testMain what n = case canonicalizeName what of

  "all"        -> runTestsAll       n

  "platform"   -> runTestsPlatform
  "bigint"     -> runTestsBigInt    n

  "stdfield"   -> runTestsStdField  n     

  "montfield"  -> runTestsMontField n
  "montgomery" -> runTestsMontField n

  "towers"     -> do 
                    runTestsExtField    n     
                    runTestsFieldTowers n
  "extfield"   -> do 
                    runTestsExtField    n     
                    runTestsFieldTowers n    

  "compare"    -> runTestsCompare n
  "againstref" -> runTestsCompare n 
  "reference"  -> runTestsCompare n

  "affcurve"    -> runTestsAffineCurve n
  "affinecurve" -> runTestsAffineCurve n
  "affine"      -> runTestsAffineCurve n
  "affcurveg2"    -> runTestsAffineCurveG2 n
  "affinecurveg2" -> runTestsAffineCurveG2 n
  "affineg2"      -> runTestsAffineCurveG2 n

  "projcurve"  -> runTestsProjCurve n
  "projective" -> runTestsProjCurve n
  "proj"       -> runTestsProjCurve n
  "projcurveg2"  -> runTestsProjCurveG2 n
  "projectiveg2" -> runTestsProjCurveG2 n
  "projg2"       -> runTestsProjCurveG2 n

  "jaccurve"      -> runTestsJacCurve n
  "jacobian"      -> runTestsJacCurve n
  "jacobiancurve" -> runTestsJacCurve n

  "poly"        -> runTestsPolys n 
  "polynomial"  -> runTestsPolys n 
  "univariate"  -> runTestsPolys n

canonicalizeName :: String -> String 
canonicalizeName = map toLower . filter isAlphaNum

isValidName :: String -> Bool
isValidName = (\s -> elem s validNames) . canonicalizeName

main :: IO ()
main = do
  args <- getArgs
  case args of

    []        -> testMain "all" 1000

    [str]     -> case readMaybe str of
                   Just n   -> testMain "all" n
                   Nothing  -> case isValidName str of
                     False    -> help
                     True     -> testMain str 1000

    [str,nnn] -> case isValidName str of
                   False    -> help
                   True     -> case readMaybe nnn of
                     Just n   -> testMain str n
                     Nothing  -> help

    _         -> help

--------------------------------------------------------------------------------
