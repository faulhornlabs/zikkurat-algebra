
module Zikkurat.Generate 
  ( HsOrC(..)
  , generate_bigints
  , generate_primefields_std
  , generate_primefields_montgomery
  , generate_curves_proj
  ) 
  where

--------------------------------------------------------------------------------

import Data.Char

import Control.Monad
import System.Environment
import System.FilePath
import System.Directory

import qualified Zikkurat.CodeGen.BigInt                as BigInt
import qualified Zikkurat.CodeGen.PrimeField.StdRep     as FpStd
import qualified Zikkurat.CodeGen.PrimeField.Montgomery as FpMont
import qualified Zikkurat.CodeGen.Curve.MontProj        as Proj

import Zikkurat.CodeGen.Misc
import Zikkurat.CodeGen.Curve.Params
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

data StdOrMont
  = Std
  | Mont
  deriving (Eq,Show)

stdMontStringReplace :: StdOrMont -> String -> String
stdMontStringReplace stdOrMont = go where
  high = show stdOrMont
  low  = map toLower high
  go ('^':'^':'^':rest) = high ++ go rest
  go (',':',':',':rest) = low  ++ go rest
  go (ch:rest)          = ch    : go rest
  go []                 = []

newtype PrimGen = PrimGen Integer
type FieldDesc  = (String,PrimGen,[String],[String],Integer) 

primefiledListReplace1 :: StdOrMont -> FieldDesc -> FieldDesc
primefiledListReplace1 stdOrMont (name,primgen,hpath,cpath,p) = (name,primgen,hpath',cpath',p) where
  hpath' = map (stdMontStringReplace stdOrMont) hpath
  cpath' = map (stdMontStringReplace stdOrMont) cpath

primefiledListReplace :: StdOrMont -> [FieldDesc] -> [FieldDesc]
primefiledListReplace stdOrMont = map (primefiledListReplace1 stdOrMont)

-- NOTE: "^^^" denotes "Std" or "Mont", while ",,," denotes "std" or "mont"
-- (we cannot use ___ for the obvious reasons.....)
primefield_list :: [FieldDesc]
primefield_list = 
  [ ( "BN128/Fp"     , PrimGen 3 , ["Curves", "BN128"    , "^^^", "Fp"] , ["curves", "fields", ",,,", "bn128_p_,,,"    ] , bn128_base_p       ) 
  , ( "BN128/Fr"     , PrimGen 5 , ["Curves", "BN128"    , "^^^", "Fr"] , ["curves", "fields", ",,,", "bn128_r_,,,"    ] , bn128_scalar_r     )
  , ( "BLS12-381/Fp" , PrimGen 2 , ["Curves", "BLS12_381", "^^^", "Fp"] , ["curves", "fields", ",,,", "bls12_381_p_,,,"] , bls12_381_base_p   ) 
  , ( "BLS12-381/Fr" , PrimGen 7 , ["Curves", "BLS12_381", "^^^", "Fr"] , ["curves", "fields", ",,,", "bls12_381_r_,,,"] , bls12_381_scalar_r )
  ]

{-
primefield_list_std = 
  [ ( ["Curves", "BN128"    , "Std", "Fp"] , ["curves", "fields", "std", "bn128_p_std"    ] , bn128_base_p       ) 
  , ( ["Curves", "BN128"    , "Std", "Fr"] , ["curves", "fields", "std", "bn128_r_std"    ] , bn128_scalar_r     )
  , ( ["Curves", "BLS12_381", "Std", "Fp"] , ["curves", "fields", "std", "bls12_381_p_std"] , bls12_381_base_p   ) 
  , ( ["Curves", "BLS12_381", "Std", "Fr"] , ["curves", "fields", "std", "bls12_381_r_std"] , bls12_381_scalar_r )
  ]

primefield_list_montgomery = 
  [ ( ["Curves", "BN128"    , "Mont", "Fp"] , ["curves", "fields", "mont", "bn128_p_mont"    ] , bn128_base_p       ) 
  , ( ["Curves", "BN128"    , "Mont", "Fr"] , ["curves", "fields", "mont", "bn128_r_mont"    ] , bn128_scalar_r     )
  , ( ["Curves", "BLS12_381", "Mont", "Fp"] , ["curves", "fields", "mont", "bls12_381_p_mont"] , bls12_381_base_p   ) 
  , ( ["Curves", "BLS12_381", "Mont", "Fr"] , ["curves", "fields", "mont", "bls12_381_r_mont"] , bls12_381_scalar_r )
  ]
-}

----------------------------------------

generate_primefields_std :: HsOrC -> FilePath -> IO ()
generate_primefields_std hsOrC tgtdir0 = do

  let primefield_list_std = primefiledListReplace Std primefield_list

  forM_ primefield_list_std $ \(name, PrimGen primgen, hpath, cpath, prime) -> do

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
          , FpStd.fieldName   = name
          , FpStd.primGen     = primgen
          }

    -- print params
    case hsOrC of 
      C  -> FpStd.primefield_std_c_codegen  tgtdir params
      Hs -> FpStd.primefield_std_hs_codegen tgtdir params

--------------------------------------------------------------------------------

generate_primefields_montgomery :: HsOrC -> FilePath -> IO ()
generate_primefields_montgomery hsOrC tgtdir0 = do

  let primefield_list_std  = primefiledListReplace Std  primefield_list
  let primefield_list_mont = primefiledListReplace Mont primefield_list

  forM_ (zip primefield_list_std primefield_list_mont) $ \( (_,PrimGen primgen,hpath_std,cpath_std,_) , (name,_,hpath,cpath,prime) ) -> do

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
    let hs_module      = "ZK.Algebra." ++ (concat $ map (++".") $ init hpath    )
    let hs_module_std  = "ZK.Algebra." ++ (concat $ map (++".") $ init hpath_std)

    let params = FpMont.Params 
          { FpMont.prefix         = last cpath     ++ "_"     -- prefix for C names
          , FpMont.stdPrefix      = last cpath_std ++ "_"     -- prefix for the C names of standard repr. version
          , FpMont.nlimbs         = nlimbs                    -- number of 64-bit limbs
          , FpMont.thePrime       = prime                     -- the prime
          , FpMont.bigint_        = bigint_                   -- the corresponding bigint prefix, like "bigint256_"
          , FpMont.c_basename     = last cpath                -- name of the @.c@ / @.h@ file (without extension)
          , FpMont.c_stdBasename  = last cpath_std            -- name of the @.c@ / @.h@ file (without extension)
          , FpMont.hs_basename    = last hpath                -- the name of the @.hs@ file (without extension) and the type too
          , FpMont.hs_module      = hs_module                 -- the module path
          , FpMont.hs_stdModule   = hs_module_std             -- the module path
          , FpMont.typeName       = last hpath                -- the name of the haskell type
          , FpMont.bigintType     = bigintType                -- the name of the haskell type of the corresponding BigInt
          , FpMont.fieldName      = name
          , FpMont.primGen        = primgen
          }

    -- print params
    case hsOrC of 
      C  -> FpMont.primefield_Montgomery_c_codegen  tgtdir params
      Hs -> FpMont.primefield_Montgomery_hs_codegen tgtdir params

--------------------------------------------------------------------------------

bls12_381_cgParams :: CodeGenParams
bls12_381_cgParams = CodeGenParams
  { prefix        = "bls12_381_G1_"                                             -- prefix for C names
  , prefix_p      = "bls12_381_p_mont_"                                         -- prefix for C names for Fp
  , prefix_r      = "bls12_381_q_mont_"                                         -- prefix for C names for Fq
  , nlimbs_p      = 6                                                           -- number of 64-bit limbs in p
  , nlimbs_r      = 4                                                           -- number of 64-bit limbs in r
  , c_path        = Path ["curves","g1","proj","bls12_381_G1_proj"]             -- path of the C file
  , hs_path       = Path ["ZK","Algebra","Curves","BLS12_381","G1","Proj"]      -- path of the Haskell module
  , hs_path_p     = Path ["ZK","Algebra","Curves","BLS12_381","Mont","Fp"]      -- path of the Haskell module for Fp
  , hs_path_r     = Path ["ZK","Algebra","Curves","BLS12_381","Mont","Fr"]      -- path of the Haskell module for Fr
  , c_basename_p  = "bls12_381_p_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r  = "bls12_381_r_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName      = "G1"                                                        -- the name of the haskell type for curve points
  }

curveList :: [(Curve,CodeGenParams)]
curveList = 
  [ ( bls12_381_curve  , bls12_381_cgParams  )
  ]

generate_curves_proj :: HsOrC -> FilePath -> IO ()
generate_curves_proj hsOrC tgtdir = do

  forM_ curveList $ \(curve,cgparams) -> do

    let ctgtdir = tgtdir </> pathDirectory (c_path  cgparams)
    let htgtdir = tgtdir </> pathDirectory (hs_path cgparams)
  
    -- only for createDirectory
    let tgtdir1 = case hsOrC of 
          C  -> ctgtdir
          Hs -> htgtdir
    createDirectoryIfMissing True tgtdir1
  
    -- print params
    case hsOrC of 
      C  -> Proj.curve_MontProj_c_codegen  tgtdir curve cgparams
      Hs -> Proj.curve_MontProj_hs_codegen tgtdir curve cgparams

--------------------------------------------------------------------------------

