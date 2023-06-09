
module Zikkurat.Generate 
  ( HsOrC(..)
  , generate_bigints
  , generate_primefields_std
  ) 
  where

--------------------------------------------------------------------------------

import Data.Char

import Control.Monad
import System.Environment
import System.FilePath
import System.Directory

import qualified Zikkurat.CodeGen.BigInt            as BigInt
import qualified Zikkurat.CodeGen.PrimeField.StdRep as FpStd

import Zikkurat.CodeGen.Misc ( HsOrC(..) )
import Zikkurat.Primes

--------------------------------------------------------------------------------

generate_bigints :: HsOrC -> FilePath -> IO ()
generate_bigints hsOrC tgtdir = do

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

primefield_list_std = 
  [ ( ["Curves", "BN128"    , "Std", "Fp"] , ["curves", "fields", "std", "bn128_p_std"    ] , bn128_base_p       ) 
  , ( ["Curves", "BN128"    , "Std", "Fr"] , ["curves", "fields", "std", "bn128_r_std"    ] , bn128_scalar_r     )
  , ( ["Curves", "BLS12_381", "Std", "Fp"] , ["curves", "fields", "std", "bls12_381_p_std"] , bls12_381_base_p   ) 
  , ( ["Curves", "BLS12_381", "Std", "Fr"] , ["curves", "fields", "std", "bls12_381_r_std"] , bls12_381_scalar_r )
  ]

generate_primefields_std :: HsOrC -> FilePath -> IO ()
generate_primefields_std hsOrC tgtdir0 = do

  forM_ primefield_list_std $ \(hpath,cpath,prime) -> do

    let ctgtdir = foldl1 (</>) (tgtdir0 : init cpath)
    let htgtdir = foldl1 (</>) (tgtdir0 : init hpath)
  
    let tgtdir = case hsOrC of 
          C  -> ctgtdir
          Hs -> htgtdir
  
    createDirectoryIfMissing True tgtdir
  
    let nlimbs = nlimbsRequired prime
    let nbits  = 64*nlimbs

    let bigint_    = "bigint" ++ show nbits ++ "_"
    let bigintType = "BigInt" ++ show nbits 
    let hs_module  = "ZK.Algebra." ++ (concat $ map (++".") $ init hpath)

    let params = FpStd.Params 
          { FpStd.prefix      = last cpath ++ "_"   -- prefix for C names
          , FpStd.nlimbs      = nlimbs              -- number of 64-bit limbs
          , FpStd.thePrime    = prime               -- the prime
          , FpStd.bigint_     = bigint_             -- the corresponding bigint prefix, like "bigint256_"
          , FpStd.c_basename  = last cpath          -- name of the @.c@ / @.h@ file (without extension)
          , FpStd.hs_basename = last hpath          -- the name of the @.hs@ file (without extension) and the type too
          , FpStd.hs_module   = hs_module           -- the module path
          , FpStd.typeName    = last hpath          -- the name of the haskell type
          , FpStd.bigintType  = bigintType          -- the name of the haskell type of the corresponding BigInt
          }

    -- print params
    case hsOrC of 
      C  -> FpStd.primefield_std_c_codegen  tgtdir params
      Hs -> FpStd.primefield_std_hs_codegen tgtdir params

--------------------------------------------------------------------------------
