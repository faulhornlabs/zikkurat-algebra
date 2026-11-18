
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module ZK.Test.Field.Ref_Tom256 where

--------------------------------------------------------------------------------

import Data.Proxy

import qualified ZK.Algebra.Pure.Instances.Tom256 as Pure_Tom256

import ZK.Algebra.Curves.Tom256.Fr.Mont (Fr) 
import ZK.Algebra.Curves.Tom256.Fp.Mont (Fp) 

import qualified ZK.Algebra.Curves.Tom256.Fr.Mont as Fast_Tom256
import qualified ZK.Algebra.Curves.Tom256.Fp.Mont as Fast_Tom256

import ZK.Test.Field.AgainstRef (runAllCmpTests)

--------------------------------------------------------------------------------

runTests_compare_Tom256 :: Int -> IO ()
runTests_compare_Tom256 n = do

  runAllCmpTests n (Proxy @Pure_Tom256.Fr  ) (Proxy @Fast_Tom256.Fr  )
  runAllCmpTests n (Proxy @Pure_Tom256.Fp  ) (Proxy @Fast_Tom256.Fp  )

--------------------------------------------------------------------------------
