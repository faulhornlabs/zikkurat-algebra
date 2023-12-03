
module Main where

--------------------------------------------------------------------------------

import Data.Char

import Text.Read

import System.Environment

import Run

--------------------------------------------------------------------------------

defaultN :: Int
defaultN = 100

--------------------------------------------------------------------------------

help :: IO ()
help = do
  putStrLn "zikkurat-algebra-pure-tests"
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ zikkurat-algebra-pure-tests [<nnn>]"
  putStrLn "$ zikkurat-algebra-pure-tests <suite> [<nnn>]"
  putStrLn ""
  putStrLn "the following testsuites are available:"
  putStrLn ""
  putStrLn " - all"
  putStrLn " - bn254"  
  putStrLn " - bls12-381"  
  putStrLn " - bls12-377"  
  putStrLn ""

--------------------------------------------------------------------------------

validNames :: [String]
validNames = 
  [ "all"
  , "bn254" , "bn128" , "altbn128"
  , "bls12381"
  , "bls12377"
  ]

testMain :: String -> Int -> IO ()
testMain what n = case canonicalizeName what of

  "all"        -> runTestsAll        n

  "bn254"      -> runTests_BN254     n     
  "bn128"      -> runTests_BN254     n     
  "altbn128"   -> runTests_BN254     n     

  "bls12381"   -> runTests_BLS12_381 n

  "bls12377"   -> runTests_BLS12_377 n

canonicalizeName :: String -> String 
canonicalizeName = map toLower . filter isAlphaNum

isValidName :: String -> Bool
isValidName = (\s -> elem s validNames) . canonicalizeName

main :: IO ()
main = do
  args <- getArgs
  case args of

    []        -> testMain "all" defaultN

    [str]     -> case readMaybe str of
                   Just n   -> testMain "all" n
                   Nothing  -> case isValidName str of
                     False    -> help
                     True     -> testMain str defaultN

    [str,nnn] -> case isValidName str of
                   False    -> help
                   True     -> case readMaybe nnn of
                     Just n   -> testMain str n
                     Nothing  -> help

    _         -> help

--------------------------------------------------------------------------------
