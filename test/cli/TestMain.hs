
module Main where

--------------------------------------------------------------------------------

import Text.Read

import System.Environment

import ZK.Test.Run

--------------------------------------------------------------------------------

help :: IO ()
help = do
  putStrLn "zikkurat-algebra-tests"
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ zikkurat-algebra-tests [<nnn>]"
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case args of

    []    -> runTests 1000
    [str] -> case readMaybe str of
               Just n   -> runTests n
               Nothing  -> help
    _     -> help

--------------------------------------------------------------------------------
