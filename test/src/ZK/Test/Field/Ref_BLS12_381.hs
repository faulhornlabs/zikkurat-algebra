
{-# LANGUAGE TypeApplications #-}
module ZK.Test.Field.Ref_BLS12_381 where

--------------------------------------------------------------------------------

import Data.Proxy

import qualified ZK.Algebra.Pure.Instances.BLS12_381 as Pure_BLS12_381

import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont   as Fast_BLS12_381
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont   as Fast_BLS12_381
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont  as Fast_BLS12_381
import qualified ZK.Algebra.Curves.BLS12_381.Fp6.Mont  as Fast_BLS12_381
import qualified ZK.Algebra.Curves.BLS12_381.Fp12.Mont as Fast_BLS12_381

import ZK.Test.Field.AgainstRef (runAllCmpTests)

--------------------------------------------------------------------------------

runTests_compare_BLS12_381 :: Int -> IO ()
runTests_compare_BLS12_381 n = do

  runAllCmpTests n (Proxy @Pure_BLS12_381.Fr  ) (Proxy @Fast_BLS12_381.Fr  )
  runAllCmpTests n (Proxy @Pure_BLS12_381.Fp  ) (Proxy @Fast_BLS12_381.Fp  )
  runAllCmpTests n (Proxy @Pure_BLS12_381.Fp2 ) (Proxy @Fast_BLS12_381.Fp2 )
  runAllCmpTests n (Proxy @Pure_BLS12_381.Fp6 ) (Proxy @Fast_BLS12_381.Fp6 )
  runAllCmpTests n (Proxy @Pure_BLS12_381.Fp12) (Proxy @Fast_BLS12_381.Fp12)

--------------------------------------------------------------------------------
