
module Zikkurat.Generate 
  ( HsOrC(..)
  , generate_bigint
  ) 
  where

--------------------------------------------------------------------------------

import Data.Char

import Control.Monad
import System.Environment
import System.FilePath
import System.Directory

import qualified Zikkurat.CodeGen.BigInt as BigInt
import Zikkurat.CodeGen.Misc ( HsOrC(..) )
import Zikkurat.Primes

--------------------------------------------------------------------------------

generate_bigint :: HsOrC -> FilePath -> IO ()
generate_bigint hsOrC tgtdir = do

  createDirectoryIfMissing True tgtdir

  case hsOrC of 
    Hs -> writeFile (tgtdir </> "Types.hs") $ unlines BigInt.hsTypesModule
    _  -> return ()

  forM_ [2..8] $ \nlimbs -> do
    let nbits = 64*nlimbs
    let params = BigInt.Params 
          { BigInt.prefix      = "bigint" ++ show nbits ++ "_"
          , BigInt.c_basename  = "bigint" ++ show nbits 
          , BigInt.hs_basename = "BigInt" ++ show nbits 
          , BigInt.hs_module   = "ZK.Algebra.BigInt." 
          , BigInt.nlimbs      = nlimbs
          }     
    -- print params
    case hsOrC of 
      C  -> BigInt.bigint_c_codegen  tgtdir params
      Hs -> BigInt.bigint_hs_codegen tgtdir params
          
--------------------------------------------------------------------------------

