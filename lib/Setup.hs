
import Control.Monad

import Distribution.Simple
import Distribution.Types.HookedBuildInfo

import System.Directory
import System.FilePath

import Zikkurat.Generate

--------------------------------------------------------------------------------

main = defaultMainWithHooks myUserHooks

myUserHooks = simpleUserHooks 
  { preBuild  = myPreBuildHook 
  , postClean = myPostCleanHook
  }

-- myPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
myPreBuildHook args buildflags = do

  generate_bigints C  "cbits" -- "/bigint"
  generate_bigints Hs "src"   -- "/ZK/Algebra/BigInt"

  generate_primefields_std C  "cbits"
  generate_primefields_std Hs "src"

  generate_primefields_montgomery C  "cbits"
  generate_primefields_montgomery Hs "src"

  generate_curves_proj C  "cbits"
  generate_curves_proj Hs "src"

  generate_curves_jac C  "cbits"
  generate_curves_jac Hs "src"

  generate_curves_affine C  "cbits"
  generate_curves_affine Hs "src"

  return $ emptyHookedBuildInfo  

myRemoveDirectory :: FilePath -> IO ()
myRemoveDirectory fpath = do
  b <- doesDirectoryExist fpath
  when b (removeDirectoryRecursive fpath)

-- myPostCleanHook :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
myPostCleanHook args cleanflags pdep mlocalbuildinfo = do
   myRemoveDirectory "src/ZK/Algebra/BigInt"
   myRemoveDirectory "src/ZK/Algebra/Curves"
   myRemoveDirectory "cbits/bigint"
   myRemoveDirectory "cbits/curves"

