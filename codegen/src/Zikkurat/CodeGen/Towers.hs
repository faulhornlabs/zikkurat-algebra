

-- | Field extension towers

{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, TypeApplications #-}
module Zikkurat.CodeGen.Towers where

--------------------------------------------------------------------------------

import Data.Word
import Data.Proxy

import Control.Monad
import Text.Printf

import ZK.Algebra.Pure.Field.Class hiding (fieldName)
import ZK.Algebra.Pure.Field.Impl

import qualified ZK.Algebra.Pure.Instances.BN254     as BN254
import qualified ZK.Algebra.Pure.Instances.BLS12_381 as BLS12_381

import Zikkurat.CodeGen.ExtField 
import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

{-

class ExtField e => Tower e where
  extParams :: Proxy e -> ExtParams

instance Tower BN254.Fp2  where extParams pxy = extParamsFp2  
instance Tower BN254.Fp6  where extParams pxy = extParamsFp6  
instance Tower BN254.Fp12 where extParams pxy = extParamsFp12 

-}

--------------------------------------------------------------------------------

allTowers :: [ExtParams]
allTowers = 
  [ extParams_bn128_Fp2
  , extParams_bn128_Fp6
  , extParams_bn128_Fp12
  , extParams_bls12_381_Fp2
  , extParams_bls12_381_Fp6
  , extParams_bls12_381_Fp12
  ]

--------------------------------------------------------------------------------

-- | NOTE: We skip the leading coefficient 1
mkAnyIrredPoly :: ExtField f => Proxy f -> AnyIrredPoly
mkAnyIrredPoly pxy = AnyIrredPoly (IrredPoly $ init $ definingPolyCoeffs pxy)

--------------------------------------------------------------------------------
-- * BN128 (or BN254, or alt-bn128) curve

extParams_bn128_Fp2 :: ExtParams
extParams_bn128_Fp2 = ExtParams 
  { prefix         = "bn128_Fp2_mont_"        --  prefix for C names
  , base_prefix    = "bn128_Fp_mont_"         --  prefix for the C names of base field
  , prime_prefix   = "bn128_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 2                        --  degree of the extension
  , primeDegree    = 2                        --  degree of the extension over the prime field
  , baseNWords     = 4                        --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 4                        --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bn128_Fp2_mont"]            --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bn128_Fp_mont" ]   
  , c_path_prime   = Path ["curves", "fields", "mont", "bn128_Fp_mont" ]   
  , hs_path        = Path ["ZK","Algebra","Curves","BN128","Fp2","Mont"]            --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BN128","Fp" ,"Mont"]            --  the module path of the std. repr. version
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BN128","Fp" ,"Mont"]             
  , typeName       = "Fp2"
  , typeNameBase   = "Fp"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BN128/Fp2"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BN254.Fp2)
  , irredPoly      = mkAnyIrredPoly (Proxy @BN254.Fp2) 
  , expoBigintType = "BigInt256"
  }  

extParams_bn128_Fp6 :: ExtParams
extParams_bn128_Fp6 = ExtParams 
  { prefix         = "bn128_Fp6_mont_"        --  prefix for C names
  , base_prefix    = "bn128_Fp2_mont_"        --  prefix for the C names of base field
  , prime_prefix   = "bn128_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 3                        --  degree of the extension
  , primeDegree    = 6                        --  degree of the extension over the prime field
  , baseNWords     = 2*4                      --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 4                        --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bn128_Fp6_mont"]             --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bn128_Fp2_mont"]             --  C path of the std. repr. version
  , c_path_prime   = Path ["curves", "fields", "mont", "bn128_Fp_mont" ]   
  , hs_path        = Path ["ZK","Algebra","Curves","BN128","Fp6","Mont"]             --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BN128","Fp2","Mont"]            
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BN128","Fp" ,"Mont"]             
  , typeName       = "Fp6"
  , typeNameBase   = "Fp2"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BN128/Fp6"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BN254.Fp6)
  , irredPoly      = mkAnyIrredPoly (Proxy @BN254.Fp6) 
  , expoBigintType = "BigInt256"
  }  

extParams_bn128_Fp12 :: ExtParams
extParams_bn128_Fp12 = ExtParams 
  { prefix         = "bn128_Fp12_mont_"       --  prefix for C names
  , base_prefix    = "bn128_Fp6_mont_"        --  prefix for the C names of base field
  , prime_prefix   = "bn128_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 2                        --  degree of the extension
  , primeDegree    = 12                       --  degree of the extension over the prime field
  , baseNWords     = 2*3*4                    --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 4                        --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bn128_Fp12_mont"]            --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bn128_Fp6_mont"]             --  C path of the std. repr. version
  , c_path_prime   = Path ["curves", "fields", "mont", "bn128_Fp_mont" ]   
  , hs_path        = Path ["ZK","Algebra","Curves","BN128","Fp12","Mont"]            --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BN128","Fp6" ,"Mont"]            
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BN128","Fp"  ,"Mont"]             
  , typeName       = "Fp12"
  , typeNameBase   = "Fp6"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BN128/Fp12"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BN254.Fp12)
  , irredPoly      = mkAnyIrredPoly (Proxy @BN254.Fp12) 
  , expoBigintType = "BigInt256"
  }  

--------------------------------------------------------------------------------
-- * BLS12-381 curve

extParams_bls12_381_Fp2 :: ExtParams
extParams_bls12_381_Fp2 = ExtParams 
  { prefix         = "bls12_381_Fp2_mont_"        --  prefix for C names
  , base_prefix    = "bls12_381_Fp_mont_"         --  prefix for the C names of base field
  , prime_prefix   = "bls12_381_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 2                            --  degree of the extension
  , primeDegree    = 2                            --  degree of the extension over the prime field
  , baseNWords     = 6                            --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 6                            --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bls12_381_Fp2_mont"]            --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bls12_381_Fp_mont" ]            --  C path of the std. repr. version
  , c_path_prime   = Path ["curves", "fields", "mont", "bls12_381_Fp_mont"]   
  , hs_path        = Path ["ZK","Algebra","Curves","BLS12_381","Fp2","Mont"]            --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BLS12_381","Fp" ,"Mont"]        
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BLS12_381","Fp" ,"Mont"]        
  , typeName       = "Fp2"
  , typeNameBase   = "Fp"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BLS12_381/Fp2"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BLS12_381.Fp2)
  , irredPoly      = mkAnyIrredPoly (Proxy @BLS12_381.Fp2) 
  , expoBigintType = "BigInt384"
  }  

extParams_bls12_381_Fp6 :: ExtParams
extParams_bls12_381_Fp6 = ExtParams 
  { prefix         = "bls12_381_Fp6_mont_"        --  prefix for C names
  , base_prefix    = "bls12_381_Fp2_mont_"        --  prefix for the C names of base field
  , prime_prefix   = "bls12_381_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 3                            --  degree of the extension
  , primeDegree    = 6                            --  degree of the extension over the prime field
  , baseNWords     = 2*6                          --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 6                            --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bls12_381_Fp6_mont"]             --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bls12_381_Fp2_mont"]             --  C path of the std. repr. version
  , c_path_prime   = Path ["curves", "fields", "mont", "bls12_381_Fp_mont"]   
  , hs_path        = Path ["ZK","Algebra","Curves","BLS12_381","Fp6","Mont"]             --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BLS12_381","Fp2","Mont"]             --  the module path of the std. repr. version
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BLS12_381","Fp" ,"Mont"]        
  , typeName       = "Fp6"
  , typeNameBase   = "Fp2"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BLS12_381/Fp6"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BLS12_381.Fp6)
  , irredPoly      = mkAnyIrredPoly (Proxy @BLS12_381.Fp6) 
  , expoBigintType = "BigInt384"
  }  

extParams_bls12_381_Fp12 :: ExtParams
extParams_bls12_381_Fp12 = ExtParams 
  { prefix         = "bls12_381_Fp12_mont_"       --  prefix for C names
  , base_prefix    = "bls12_381_Fp6_mont_"        --  prefix for the C names of base field
  , prime_prefix   = "bls12_381_Fp_mont_"         --  prefix for the C names of prime field
  , extDegree      = 2                            --  degree of the extension
  , primeDegree    = 12                           --  degree of the extension over the prime field
  , baseNWords     = 2*3*6                        --  size of the base-field elements, in number of 64-bit words 
  , primeNWords    = 6                            --  size of the prime-field elements, in number of 64-bit words 
  , c_path         = Path ["curves", "fields", "mont", "bls12_381_Fp12_mont"]            --  path of the C module (without extension)
  , c_path_base    = Path ["curves", "fields", "mont", "bls12_381_Fp6_mont"]   
  , c_path_prime   = Path ["curves", "fields", "mont", "bls12_381_Fp_mont"]   
  , hs_path        = Path ["ZK","Algebra","Curves","BLS12_381","Fp12","Mont"]            --  path of the Hs module
  , hs_path_base   = Path ["ZK","Algebra","Curves","BLS12_381","Fp6" ,"Mont"]            --  the module path of the std. repr. version
  , hs_path_prime  = Path ["ZK","Algebra","Curves","BLS12_381","Fp"  ,"Mont"]        
  , typeName       = "Fp12"
  , typeNameBase   = "Fp6"
  , typeNamePrime  = "Fp"
  , extFieldName   = "BLS12_381/Fp12"
  , pureTypeProxy  = AnyExtProxy    (Proxy @BLS12_381.Fp12)
  , irredPoly      = mkAnyIrredPoly (Proxy @BLS12_381.Fp12) 
  , expoBigintType = "BigInt384"
  }  

--------------------------------------------------------------------------------
