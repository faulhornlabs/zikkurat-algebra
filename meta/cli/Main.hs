
module Main where

--------------------------------------------------------------------------------

import Data.Char

import Control.Monad
import System.Environment

-- import Paths_zikkurat_algebra_codegen (version)
-- import Data.Version (showVersion)

import Zikkurat.CodeGen.Misc ( HsOrC(..) , parseHsOrC )
import Zikkurat.Generate as Gen
import Zikkurat.Primes

--------------------------------------------------------------------------------

-- myVersion :: String
-- myVersion = showVersion version

help :: IO ()
help = putStrLn $ unlines
  [ "zikkurat-algebra-codegen " -- ++ myVersion
  , ""
  , "usage:"
  , "======"
  , ""
  , "$ zikkurat-algebra-codegen [c|hs] bigint      <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] fields      <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] curves      <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] testfields  <tgt_dir>"
  ]

--------------------------------------------------------------------------------

main1 :: HsOrC -> String -> FilePath -> IO ()
main1 hsOrC what tgtdir = case map toLower what of
  "bigint" -> Gen.generate_bigints         hsOrC tgtdir 
  "fields" -> Gen.generate_primefields_std hsOrC tgtdir 
  _        -> help

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                 -> help
    ("-h"    :_)       -> help
    ("--help":_)       -> help
    [lang,what,tgtdir] -> case parseHsOrC lang of
      Just hsOrC         -> main1 hsOrC what tgtdir
      Nothing            -> help
    _                  -> help 

--------------------------------------------------------------------------------
