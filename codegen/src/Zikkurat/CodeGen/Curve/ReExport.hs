
{-# LANGUAGE RecordWildCards #-}
module Zikkurat.CodeGen.Curve.ReExport where

--------------------------------------------------------------------------------

import Data.List
import System.FilePath

import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data Renamed 
  = Keep   String                          -- ^ eg. @Keep "Fp"@
  | Rename String String                   -- ^ eg. @Rename "Fr" "StdFr"@
  deriving (Eq,Show)

renamedOld :: Renamed -> String
renamedOld (Keep   old    ) = old
renamedOld (Rename old new) = old

renamedNew :: Renamed -> String
renamedNew (Keep   old    ) = old
renamedNew (Rename old new) = new

data ExportedModule = ExportedModule
  { moduleSuffix   :: String              -- ^ eg. @"Fp.Mont"@
  , exportedTypes  :: [Renamed]           -- ^ eg. @[Keep "Fp"]@
  , exportedValues :: [Renamed]           -- ^ eg. @[Keep "pairing"]@
  }
  deriving (Eq,Show)

exportedModule_ suffix types = ExportedModule suffix types []

data ReExportParams = ReExportParams 
  { hsPath       :: Path                  -- ^ eg. @Path ["ZK","Algebra","Curves","BN128"]@
  , moduleName   :: String                -- ^ eg. @"BLS12_381"@
  , moduleList   :: [ExportedModule]      -- re-export list
  }
  deriving (Eq,Show)

reexports_standardPairingCurve :: [ExportedModule]
reexports_standardPairingCurve =
  [ exportedModule_ "Fp.Mont"   [Keep "Fp"]   
  , exportedModule_ "Fp2.Mont"  [Keep "Fp2"] 
  , exportedModule_ "Fp12.Mont" [Keep "Fp12"] 
  , exportedModule_ "Fr.Mont"   [Keep "Fr"] 
  , exportedModule_ "Fr.Std"    [Rename "Fr" "StdFr"] 
  , exportedModule_ "G1.Affine" [Rename "G1" "G1Affine"] 
  , exportedModule_ "G2.Affine" [Rename "G2" "G2Affine"] 
  , exportedModule_ "G1.Proj"   [Rename "G1" "G1Proj"] 
  , exportedModule_ "G2.Proj"   [Rename "G2" "G2Proj"] 
  , exportedModule_ "Poly"      [Keep "Poly"] 
  , exportedModule_ "Array"     []
  , ExportedModule  "Pairing"   []     
                                [Keep "pairing"]
  ]

--------------------------------------------------------------------------------

hs_code :: ReExportParams -> Code
hs_code params@(ReExportParams{..}) = 
  [ "-- | Convenient reexport of (monomorphic) types for " ++ moduleName 
  , ""
  , "module " ++ hsModule hsPath++ " where"
  , ""
  , "--------------------------------------------------------------------------------"
  , ""
  ] ++ 
  concatMap (mkImport params) moduleList ++
  [ ""
  , "--------------------------------------------------------------------------------"
  , ""
  ] ++
  concatMap (mkTypes params) moduleList ++
  [ ""
  , "--------------------------------------------------------------------------------"
  , ""
  ] ++
  concatMap (mkValues params) moduleList ++
  [ "" 
  , "--------------------------------------------------------------------------------"
  , ""
  ]

mkImport :: ReExportParams -> ExportedModule -> Code
mkImport (ReExportParams{..}) (ExportedModule{..}) =
  [ "import qualified " ++ hsModule hsPath ++ "." ++ moduleSuffix
  ]

mkTypes :: ReExportParams -> ExportedModule -> Code
mkTypes (ReExportParams{..}) (ExportedModule{..}) =
  [ "type " ++ renamedNew ren ++ " = " ++ hsModule hsPath ++ "." ++ moduleSuffix ++ "." ++ renamedOld ren
  | ren <- exportedTypes
  ]

mkValues :: ReExportParams -> ExportedModule -> Code
mkValues (ReExportParams{..}) (ExportedModule{..}) =
  [ renamedNew ren ++ " = " ++ hsModule hsPath ++ "." ++ moduleSuffix ++ "." ++ renamedOld ren
  | ren <- exportedValues
  ]

--------------------------------------------------------------------------------

curve_reexport_hs_codegen :: FilePath -> ReExportParams -> IO ()
curve_reexport_hs_codegen tgtdir params@(ReExportParams{..}) = do

  let fn_hs = tgtdir </> (hsFilePath hsPath)

  createTgtDirectory fn_hs

  putStrLn $ "writing `" ++ fn_hs ++ "`" 
  writeFile fn_hs $ unlines $ hs_code params

--------------------------------------------------------------------------------
