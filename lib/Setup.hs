
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

  generate_bigints C  "cbits/bigint"
  generate_bigints Hs "src/ZK/Algebra/BigInt"

  generate_primefields_std C  "cbits"
  generate_primefields_std Hs "src/ZK/Algebra"

  return $ emptyHookedBuildInfo  

-- myPostCleanHook :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
myPostCleanHook args cleanflags pdep mlocalbuildinfo = do
   removeDirectoryRecursive "src/ZK/Algebra/BigInt"
   removeDirectoryRecursive "src/ZK/Algebra/Curves"
   removeDirectoryRecursive "cbits/bigint"
   removeDirectoryRecursive "cbits/curves"

