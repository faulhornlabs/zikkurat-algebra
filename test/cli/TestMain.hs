
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
  putStrLn " - bigint"
  putStrLn " - std_field"
  putStrLn " - montgomery"
  putStrLn " - affine_curve"
  putStrLn " - proj_curve"
  putStrLn " - jac_curve"
  putStrLn " - poly"
  putStrLn ""

--------------------------------------------------------------------------------

validNames :: [String]
validNames = 
  [ "all"
  , "bigint"
  , "stdfield"
  , "montfield" , "montgomery"
  , "projcurve" , "projective"
  , "jaccurve" , "jacobiancurve" , "jacobian"
  , "affcurve" , "affinecurve" , "affine"
  , "poly" , "polynomial" , "univariate"
  ]

testMain :: String -> Int -> IO ()
testMain what n = case canonicalizeName what of

  "all"        -> runTestsAll       n
  "bigint"     -> runTestsBigInt    n
  "stdfield"   -> runTestsStdField  n     

  "montfield"  -> runTestsMontField n
  "montgomery" -> runTestsMontField n

  "projcurve"  -> runTestsProjCurve n
  "projective" -> runTestsProjCurve n

  "jaccurve"      -> runTestsJacCurve n
  "jacobian"      -> runTestsJacCurve n
  "jacobiancurve" -> runTestsJacCurve n

  "affcurve"    -> runTestsAffineCurve n
  "affinecurve" -> runTestsAffineCurve n
  "affine"      -> runTestsAffineCurve n

  "poly"        -> runTestsPolys n 
  "polynomial"  -> runTestsPolys n 
  "univariate"  -> runTestsPolys n

canonicalizeName :: String -> String 
canonicalizeName = map toLower . filter isLetter

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
