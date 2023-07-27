
module Zikkurat.Generate 
  ( HsOrC(..)
  , generate_bigints
  , generate_primefields_std
  , generate_primefields_montgomery
  , generate_curves_affine
  , generate_curves_proj
  , generate_curves_jac
  , generate_curves_poly
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
import qualified Zikkurat.CodeGen.Curve.MontAffine      as Affine
import qualified Zikkurat.CodeGen.Curve.MontProj        as Proj
import qualified Zikkurat.CodeGen.Curve.MontJac         as Jac
import qualified Zikkurat.CodeGen.Poly                  as Poly

import Zikkurat.CodeGen.Misc
import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Poly ( PolyParams )
import Zikkurat.Primes

--------------------------------------------------------------------------------

generate_bigints :: HsOrC -> FilePath -> IO ()
generate_bigints hsOrC tgtdir = do

  let c_tgtdir  = tgtdir </> "bigint"
  let hs_tgtdir = tgtdir </> "ZK/Algebra/BigInt"

  case hsOrC of
    C  -> createDirectoryIfMissing True c_tgtdir
    Hs -> createDirectoryIfMissing True hs_tgtdir

  case hsOrC of 
    Hs -> writeFile (hs_tgtdir </> "Types.hs") $ unlines BigInt.hsTypesModule
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
      C  -> BigInt.bigint_c_codegen  c_tgtdir  params
      Hs -> BigInt.bigint_hs_codegen hs_tgtdir params
          
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

newtype PrimGen   = PrimGen Integer
type    DomainGen = (Int,Integer)
type FieldDesc  = (String,PrimGen,Maybe DomainGen,[String],[String],String,Integer) 

primefiledListReplace1 :: StdOrMont -> FieldDesc -> FieldDesc
primefiledListReplace1 stdOrMont (name,primgen,mbfft,hpath,cpath,typeName,p) = (name,primgen,mbfft,hpath',cpath',typeName,p) where
  hpath' = map (stdMontStringReplace stdOrMont) hpath
  cpath' = map (stdMontStringReplace stdOrMont) cpath

primefiledListReplace :: StdOrMont -> [FieldDesc] -> [FieldDesc]
primefiledListReplace stdOrMont = map (primefiledListReplace1 stdOrMont)

-- NOTE: "^^^" denotes "Std" or "Mont", while ",,," denotes "std" or "mont"
-- (we cannot use ___ for the obvious reasons.....)
primefield_list :: [FieldDesc]
primefield_list = 
  [ ( "BN128/Fp"     , PrimGen 3 , Nothing               , ["ZK","Algebra","Curves", "BN128"    , "Fp", "^^^" ] , ["curves", "fields", ",,,", "bn128_p_,,,"    ] , "Fp" , bn128_base_p       ) 
  , ( "BN128/Fr"     , PrimGen 5 , Just domain_BN128     , ["ZK","Algebra","Curves", "BN128"    , "Fr", "^^^" ] , ["curves", "fields", ",,,", "bn128_r_,,,"    ] , "Fr" , bn128_scalar_r     )
  , ( "BLS12-381/Fp" , PrimGen 2 , Nothing               , ["ZK","Algebra","Curves", "BLS12_381", "Fp", "^^^" ] , ["curves", "fields", ",,,", "bls12_381_p_,,,"] , "Fp" , bls12_381_base_p   ) 
  , ( "BLS12-381/Fr" , PrimGen 7 , Just domain_BLS12_381 , ["ZK","Algebra","Curves", "BLS12_381", "Fr", "^^^" ] , ["curves", "fields", ",,,", "bls12_381_r_,,,"] , "Fr" , bls12_381_scalar_r )
  ]

-- fft domains
domain_BN128     = (28,19103219067921713944291392827692070036145651957329286315305642004821462161904)
domain_BLS12_381 = (32,10238227357739495823651030575849232062558860180284477541189508159991286009131)

----------------------------------------

generate_primefields_std :: HsOrC -> FilePath -> IO ()
generate_primefields_std hsOrC tgtdir = do

  let primefield_list_std = primefiledListReplace Std primefield_list

  forM_ primefield_list_std $ \(name, PrimGen primgen, mbDomainGen, hpath, cpath, typeName, prime) -> do
  
    let nlimbs = nlimbsRequired prime
    let nbits  = 64*nlimbs

    let bigint_    = "bigint" ++ show nbits ++ "_"
    let bigintType = "BigInt" ++ show nbits 

    let params = FpStd.Params 
          { FpStd.prefix      = last cpath ++ "_"   -- prefix for C names
          , FpStd.nlimbs      = nlimbs              -- number of 64-bit limbs
          , FpStd.thePrime    = prime               -- the prime
          , FpStd.bigint_     = bigint_             -- the corresponding bigint prefix, like "bigint256_"
          , FpStd.c_path      = Path cpath          -- name of the @.c@ / @.h@ file (without extension)
          , FpStd.hs_path     = Path hpath          -- the name of the @.hs@ file (without extension) and the type too
          , FpStd.typeName    = typeName            -- the name of the haskell type
          , FpStd.bigintType  = bigintType          -- the name of the haskell type of the corresponding BigInt
          , FpStd.fieldName   = name
          , FpStd.primGen     = primgen
          , FpStd.fftDomain   = mbDomainGen
          }

    -- print params
    case hsOrC of 
      C  -> FpStd.primefield_std_c_codegen  tgtdir params
      Hs -> FpStd.primefield_std_hs_codegen tgtdir params

--------------------------------------------------------------------------------

generate_primefields_montgomery :: HsOrC -> FilePath -> IO ()
generate_primefields_montgomery hsOrC tgtdir = do

  let primefield_list_std  = primefiledListReplace Std  primefield_list
  let primefield_list_mont = primefiledListReplace Mont primefield_list

  forM_ (zip primefield_list_std primefield_list_mont) $ \( (_,PrimGen primgen,mbDomainGen,hpath_std,cpath_std,_typeName,_prime) , (name,_,_,hpath,cpath,typeName,prime) ) -> do
  
    let nlimbs = nlimbsRequired prime
    let nbits  = 64*nlimbs

    let bigint_    = "bigint" ++ show nbits ++ "_"
    let bigintType = "BigInt" ++ show nbits 

    let params = FpMont.Params 
          { FpMont.prefix         = last cpath     ++ "_"     -- prefix for C names
          , FpMont.stdPrefix      = last cpath_std ++ "_"     -- prefix for the C names of standard repr. version
          , FpMont.nlimbs         = nlimbs                    -- number of 64-bit limbs
          , FpMont.thePrime       = prime                     -- the prime
          , FpMont.bigint_        = bigint_                   -- the corresponding bigint prefix, like "bigint256_"
          , FpMont.c_path         = Path cpath                -- name of the @.c@ / @.h@ file (without extension)
          , FpMont.c_path_std     = Path cpath_std            -- name of the @.c@ / @.h@ file (without extension)
          , FpMont.hs_path        = Path hpath                -- the name of the @.hs@ file (without extension) and the type too
          , FpMont.hs_path_std    = Path hpath_std            -- the module path
          , FpMont.typeName       = typeName                  -- the name of the haskell type
          , FpMont.bigintType     = bigintType                -- the name of the haskell type of the corresponding BigInt
          , FpMont.fieldName      = name
          , FpMont.primGen        = primgen
          , FpMont.fftDomain      = mbDomainGen
          }

    -- print params
    case hsOrC of 
      C  -> FpMont.primefield_Montgomery_c_codegen  tgtdir params
      Hs -> FpMont.primefield_Montgomery_hs_codegen tgtdir params

--------------------------------------------------------------------------------

bn128_cgParams :: CodeGenParams
bn128_cgParams = CodeGenParams
  { prefix         = error "bn128 / prefix"                                      -- prefix for C names
  , prefix_affine  = "bn128_G1_affine_"                                          -- prefix for C names
  , prefix_proj    = "bn128_G1_proj_"                                            -- prefix for C names
  , prefix_jac     = "bn128_G1_jac_"                                             -- prefix for C names
  , prefix_p       = "bn128_p_mont_"                                             -- prefix for C names for Fp
  , prefix_r       = "bn128_r_mont_"                                             -- prefix for C names for Fq
  , point_repr     = error "bn128 / point_repr"                                  -- one of "affine", "proj" or "jac"
  , nlimbs_p       = 4                                                           -- number of 64-bit limbs in p
  , nlimbs_r       = 4                                                           -- number of 64-bit limbs in r
  , hs_path_p      = Path ["ZK","Algebra","Curves","BN128","Fp","Mont"]          -- path of the Haskell module for Fp
  , hs_path_r      = Path ["ZK","Algebra","Curves","BN128","Fr","Mont"]          -- path of the Haskell module for Fr
  , hs_path_r_std  = Path ["ZK","Algebra","Curves","BN128","Fr","Std"]           -- path of the Haskell module for Fr (standard repr)
  , hs_path_big_p  = Path ["ZK","Algebra","BigInt","BigInt256"]                  
  , c_path         = error "bn128 / c_path"
  , c_path_affine  = Path ["curves","g1","affine","bn128_G1_affine"]               -- path of the C file
  , c_path_proj    = Path ["curves","g1","proj"  ,"bn128_G1_proj"]                 -- path of the C file
  , c_path_jac     = Path ["curves","g1","jac"   ,"bn128_G1_jac"]                  -- path of the C file
  , hs_path        = error "bn128 / hs_path"
  , hs_path_affine = Path ["ZK","Algebra","Curves","BN128","G1","Affine"]        -- path of the Haskell module
  , hs_path_proj   = Path ["ZK","Algebra","Curves","BN128","G1","Proj"]          -- path of the Haskell module
  , hs_path_jac    = Path ["ZK","Algebra","Curves","BN128","G1","Jac"]           -- path of the Haskell module
  , c_basename_p   = "bn128_p_mont"                                              -- name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r   = "bn128_r_mont"                                              -- name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName       = "G1"                                                        -- the name of the haskell type for curve points
  }

bn128_polyParams :: PolyParams
bn128_polyParams = Poly.PolyParams 
  { Poly.prefix     = "bn128_poly_mont_"   
  , Poly.prefix_r   = "bn128_r_mont_"
  , Poly.nlimbs     = 4 
  , Poly.c_path     = Path ["curves","poly"  , "mont", "bn128_poly_mont" ]
  , Poly.c_path_r   = Path ["curves","fields", "mont", "bn128_r_mont" ]
  , Poly.hs_path    = Path ["ZK","Algebra","Curves","BN128","Poly"]
  , Poly.hs_path_r  = Path ["ZK","Algebra","Curves","BN128","Fr","Mont"] 
  , Poly.typeName   = "Poly" 
  , Poly.typeName_r = "Fr"
  , Poly.prime_r    = bn128_scalar_r   
  }

bls12_381_cgParams :: CodeGenParams
bls12_381_cgParams = CodeGenParams
  { prefix         = error "bls12_381 / prefix"                                  -- prefix for C names
  , prefix_affine  = "bls12_381_G1_affine_"                                      -- prefix for C names
  , prefix_proj    = "bls12_381_G1_proj_"                                        -- prefix for C names
  , prefix_jac     = "bls12_381_G1_jac_"                                         -- prefix for C names
  , prefix_p       = "bls12_381_p_mont_"                                         -- prefix for C names for Fp
  , prefix_r       = "bls12_381_r_mont_"                                         -- prefix for C names for Fq
  , point_repr     = error "bn128 / point_repr"                                  -- one of "affine", "proj" or "jac"
  , nlimbs_p       = 6                                                           -- number of 64-bit limbs in p
  , nlimbs_r       = 4                                                           -- number of 64-bit limbs in r
  , hs_path_p      = Path ["ZK","Algebra","Curves","BLS12_381","Fp","Mont"]      -- path of the Haskell module for Fp
  , hs_path_r      = Path ["ZK","Algebra","Curves","BLS12_381","Fr","Mont"]      -- path of the Haskell module for Fr
  , hs_path_r_std  = Path ["ZK","Algebra","Curves","BLS12_381","Fr","Std"]       -- path of the Haskell module for Fr (standard repr)
  , hs_path_big_p  = Path ["ZK","Algebra","BigInt","BigInt384"]                  
  , c_path         = error "bls12_381 / c_path"
  , c_path_affine  = Path ["curves","g1","affine","bls12_381_G1_affine"]           -- path of the C file
  , c_path_proj    = Path ["curves","g1","proj"  ,"bls12_381_G1_proj"]             -- path of the C file
  , c_path_jac     = Path ["curves","g1","jac"   ,"bls12_381_G1_jac"]              -- path of the C file
  , hs_path        = error "bls12_381 / hs_path"
  , hs_path_affine = Path ["ZK","Algebra","Curves","BLS12_381","G1","Affine"]    -- path of the Haskell module
  , hs_path_proj   = Path ["ZK","Algebra","Curves","BLS12_381","G1","Proj"]      -- path of the Haskell module
  , hs_path_jac    = Path ["ZK","Algebra","Curves","BLS12_381","G1","Jac"]       -- path of the Haskell module
  , c_basename_p   = "bls12_381_p_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r   = "bls12_381_r_mont"                                          -- name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName       = "G1"                                                        -- the name of the haskell type for curve points
  }

bls12_381_polyParams :: PolyParams
bls12_381_polyParams = Poly.PolyParams 
  { Poly.prefix     = "bls12_381_poly_mont_"   
  , Poly.prefix_r   = "bls12_381_r_mont_"
  , Poly.nlimbs     = 4 
  , Poly.c_path     = Path ["curves","poly"  , "mont", "bls12_381_poly_mont" ]
  , Poly.c_path_r   = Path ["curves","fields", "mont", "bls12_381_r_mont" ]
  , Poly.hs_path    = Path ["ZK","Algebra","Curves","BLS12_381","Poly"]
  , Poly.hs_path_r  = Path ["ZK","Algebra","Curves","BLS12_381","Fr","Mont"] 
  , Poly.typeName   = "Poly" 
  , Poly.typeName_r = "Fr"
  , Poly.prime_r    = bls12_381_scalar_r
  }

--------------------------------------------------------------------------------

generate_curves_poly :: HsOrC -> FilePath -> IO ()
generate_curves_poly hsOrC tgtdir = do
  forM_ curveList $ \(curve,_,polyparams) -> do
    case hsOrC of 
      C  -> Poly.poly_c_codegen  tgtdir polyparams
      Hs -> Poly.poly_hs_codegen tgtdir polyparams

--------------------------------------------------------------------------------

curveList :: [(Curve,CodeGenParams,PolyParams)]
curveList = 
  [ ( bn128_curve     , bn128_cgParams     , bn128_polyParams     )
  , ( bls12_381_curve , bls12_381_cgParams , bls12_381_polyParams )
  ]

generate_curves_proj :: HsOrC -> FilePath -> IO ()
generate_curves_proj hsOrC tgtdir = do
  forM_ curveList $ \(curve,cgparams0,_) -> do
    let cgparams = cgparams0 
          { prefix  = prefix_proj  cgparams0
          , c_path  = c_path_proj  cgparams0 
          , hs_path = hs_path_proj cgparams0 
          , point_repr = "proj"
          }
    case hsOrC of 
      C  -> Proj.curve_MontProj_c_codegen  tgtdir curve cgparams
      Hs -> Proj.curve_MontProj_hs_codegen tgtdir curve cgparams

generate_curves_jac :: HsOrC -> FilePath -> IO ()
generate_curves_jac hsOrC tgtdir = do
  forM_ curveList $ \(curve,cgparams0,_) -> do
    let cgparams = cgparams0 
          { prefix  = prefix_jac  cgparams0
          , c_path  = c_path_jac  cgparams0 
          , hs_path = hs_path_jac cgparams0 
          , point_repr = "jac"
          }
    case hsOrC of 
      C  -> Jac.curve_MontJac_c_codegen  tgtdir curve cgparams
      Hs -> Jac.curve_MontJac_hs_codegen tgtdir curve cgparams

generate_curves_affine :: HsOrC -> FilePath -> IO ()
generate_curves_affine hsOrC tgtdir = do
  forM_ curveList $ \(curve,cgparams0,_) -> do
    let cgparams = cgparams0 
          { prefix  = prefix_affine  cgparams0 
          , c_path  = c_path_affine  cgparams0 
          , hs_path = hs_path_affine cgparams0 
          , point_repr = "affine"
          }
    case hsOrC of 
      C  -> Affine.curve_MontAffine_c_codegen  tgtdir curve cgparams
      Hs -> Affine.curve_MontAffine_hs_codegen tgtdir curve cgparams

--------------------------------------------------------------------------------

