
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module ZK.Test.Field.Ref_BN254 where

--------------------------------------------------------------------------------

import Data.Proxy

import qualified ZK.Algebra.Pure.Instances.BN254   as Pure_BN254

import ZK.Algebra.Curves.BN128.Fr.Mont   (Fr  ) 
import ZK.Algebra.Curves.BN128.Fp.Mont   (Fp  ) 
import ZK.Algebra.Curves.BN128.Fp2.Mont  (Fp2 ) 
import ZK.Algebra.Curves.BN128.Fp6.Mont  (Fp6 ) 
import ZK.Algebra.Curves.BN128.Fp12.Mont (Fp12) 

import qualified ZK.Algebra.Curves.BN128.Fr.Mont   as Fast_BN254
import qualified ZK.Algebra.Curves.BN128.Fp.Mont   as Fast_BN254
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont  as Fast_BN254
import qualified ZK.Algebra.Curves.BN128.Fp6.Mont  as Fast_BN254
import qualified ZK.Algebra.Curves.BN128.Fp12.Mont as Fast_BN254

import ZK.Test.Field.AgainstRef (runAllCmpTests)

--------------------------------------------------------------------------------

runTests_compare_BN254 :: Int -> IO ()
runTests_compare_BN254 n = do

  runAllCmpTests n (Proxy @Pure_BN254.Fr  ) (Proxy @Fast_BN254.Fr  )
  runAllCmpTests n (Proxy @Pure_BN254.Fp  ) (Proxy @Fast_BN254.Fp  )
  runAllCmpTests n (Proxy @Pure_BN254.Fp2 ) (Proxy @Fast_BN254.Fp2 )
  runAllCmpTests n (Proxy @Pure_BN254.Fp6 ) (Proxy @Fast_BN254.Fp6 )
  runAllCmpTests n (Proxy @Pure_BN254.Fp12) (Proxy @Fast_BN254.Fp12)

--------------------------------------------------------------------------------

-- foo = do
--   baz (Proxy @Pure_BN254.Fp6  ) (Proxy @Fast_BN254.Fp6  )

--------------------------------------------------------------------------------
