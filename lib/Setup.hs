
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
  generate_bigint C  "cbits/bigint"
  generate_bigint Hs "src/ZK/Algebra/BigInt"
  return $ emptyHookedBuildInfo  

-- myPostCleanHook :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
myPostCleanHook args cleanflags pdep mlocalbuildinfo = do
   removeDirectoryRecursive "src/ZK/Algebra/BigInt"
   removeDirectoryRecursive "cbits/bigint"

