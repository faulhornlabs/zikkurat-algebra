
module Main where

--------------------------------------------------------------------------------

import Data.Char

import Control.Monad

import System.Environment
import System.FilePath

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
  , "$ zikkurat-algebra-codegen [c|hs] towers      <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] curves      <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] poly        <tgt_dir>"
  , "$ zikkurat-algebra-codegen [c|hs] testfields  <tgt_dir>"
  , "$ zikkurat-algebra-codegen        all         <tgt_dir>"
  ]

--------------------------------------------------------------------------------

main1 :: HsOrC -> String -> FilePath -> IO ()
main1 hsOrC what tgtdir = case map toLower what of

  "bigint" -> do Gen.generate_platform               hsOrC tgtdir
                 Gen.generate_bigints                hsOrC tgtdir 

  "fields" -> do Gen.generate_primefields_std        hsOrC tgtdir 
                 Gen.generate_primefields_montgomery hsOrC tgtdir 

  "towers" -> do Gen.generate_extfield_towers        hsOrC tgtdir    

  "curves" -> do Gen.generate_curves_proj            hsOrC tgtdir 
                 Gen.generate_curves_jac             hsOrC tgtdir 
                 Gen.generate_curves_affine          hsOrC tgtdir 

  "poly"   -> do Gen.generate_curves_poly            hsOrC tgtdir

  _        ->    help

----------------------------------------

mainAll :: FilePath -> IO ()
mainAll tgtdir = do
  mainAllWhich C  (tgtdir </> "cbits")
  mainAllWhich Hs (tgtdir </> "src"  )

mainAllWhich :: HsOrC -> FilePath -> IO ()
mainAllWhich hsOrC tgtdir = do

  Gen.generate_platform               hsOrC tgtdir
  Gen.generate_bigints                hsOrC tgtdir

  Gen.generate_primefields_std        hsOrC tgtdir 
  Gen.generate_primefields_montgomery hsOrC tgtdir 

  Gen.generate_extfield_towers        hsOrC tgtdir    

  Gen.generate_curves_proj            hsOrC tgtdir 
  Gen.generate_curves_jac             hsOrC tgtdir 
  Gen.generate_curves_affine          hsOrC tgtdir 

  Gen.generate_curves_poly            hsOrC tgtdir

----------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case map (map toLower) args of
    []                 -> help
    ("-h"    :_)       -> help
    ("--help":_)       -> help
    ["all",tgtdir]     -> mainAll tgtdir 
    [lang,what,tgtdir] -> case parseHsOrC lang of
      Just hsOrC         -> main1 hsOrC what tgtdir
      Nothing            -> help
    _                  -> help 

--------------------------------------------------------------------------------
