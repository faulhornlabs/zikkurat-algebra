
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module ZK.Test.Field.Ref_P256 where

--------------------------------------------------------------------------------

import Data.Proxy

import qualified ZK.Algebra.Pure.Instances.P256 as Pure_P256

import ZK.Algebra.Curves.P256.Fr.Mont (Fr) 
import ZK.Algebra.Curves.P256.Fp.Mont (Fp) 

import qualified ZK.Algebra.Curves.BN128.Fr.Mont as Fast_P256
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as Fast_P256

import ZK.Test.Field.AgainstRef (runAllCmpTests)

--------------------------------------------------------------------------------

runTests_compare_P256 :: Int -> IO ()
runTests_compare_P256 n = do

  runAllCmpTests n (Proxy @Pure_P256.Fr  ) (Proxy @Fast_P256.Fr  )
  runAllCmpTests n (Proxy @Pure_P256.Fp  ) (Proxy @Fast_P256.Fp  )

--------------------------------------------------------------------------------
