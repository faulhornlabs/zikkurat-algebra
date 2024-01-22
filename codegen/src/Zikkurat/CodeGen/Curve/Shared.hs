
-- | Hackety hack refactoring

{-# LANGUAGE StrictData, RecordWildCards #-}
module Zikkurat.CodeGen.Curve.Shared where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc

import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Curve.CurveFFI

--------------------------------------------------------------------------------

x_curvename :: XCurve -> String
x_curvename xcurve = curveName (extractCurve1 xcurve)

full_curvename :: XCurve -> String
full_curvename xcurve = case xcurve of
  Left  _ -> x_curvename xcurve ++ " ( Fp ) "
  Right _ -> x_curvename xcurve ++ " ( Fp2 ) "

x_groupname :: XCurve -> String
x_groupname xcurve= case xcurve of
  Left  _ -> x_curvename xcurve ++ " / G1 "
  Right _ -> x_curvename xcurve ++ " / G2 "

--------------------------------------------------------------------------------

c_begin_curve1 :: Bool -> Curve1 -> CodeGenParams -> Code
c_begin_curve1 is_proj (Curve1{..}) cgparams@(CodeGenParams{..}) =
  [ "// the generator of the subgroup G1"
  , mkConstArr nlimbs_p (prefix ++ "gen_G1") (map toMontgomery coords)
  , ""
  , "// the cofactor of the curve subgroup = " ++ show cofactor
  , mkConst nlimbs_p (prefix ++ "cofactor") cofactor
  , ""
  , "// the constants A and B of the equation"
  , mkConst nlimbs_p (prefix ++ "const_A" ) (toMontgomery curveA)
  , mkConst nlimbs_p (prefix ++ "const_B" ) (toMontgomery curveB)
  , mkConst nlimbs_p (prefix ++ "const_3B") (toMontgomery curve3B)
  ]
  where
    curve3B = mod (3*curveB) curveFp
    toMontgomery x = mod ( 2^(64*nlimbs_p) * x ) curveFp
    coords = [ fst subgroupGen, snd subgroupGen ] ++ if is_proj then [1] else []

c_begin_curve2 :: Bool -> Curve12 -> CodeGenParams -> Code
c_begin_curve2 is_proj (Curve12 curve1 curve2@(Curve2{..})) cgparams@(CodeGenParams{..}) =
  [ "// the generator of the subgroup G2"
  , mkConstArrFp2 nlimbs_p (prefix ++ "gen_G2") (map toMontgomeryFp2 coords)
  , ""
  , "// the cofactor of the curve subgroup = " ++ show g2_cofactor
  , mkConst nlimbs_p (prefix ++ "cofactor") g2_cofactor
  , ""
  , "// the constants A and B of the equation"
  , mkConstFp2 nlimbs_p (prefix ++ "const_A")  (toMontgomeryFp2 g2_curveA)
  , mkConstFp2 nlimbs_p (prefix ++ "const_B")  (toMontgomeryFp2 g2_curveB)
  , mkConstFp2 nlimbs_p (prefix ++ "const_3B") (toMontgomeryFp2 g2_curve3B)
  ]
  where
    p = curveFp curve1
    nlimbs_p_orig  = div nlimbs_p 2       -- ugly hacks galore... nlimbs_p is for Fp2...
    toMontgomery x = mod ( 2^(64*nlimbs_p_orig) * x ) p
    toMontgomeryFp2 (x,y) = (toMontgomery x, toMontgomery y)
    coords = [ fst g2_subgroupGen, snd g2_subgroupGen ] ++ if is_proj then [(1,0)] else [(0,0)]
    g2_curve3B = pairMap (\x -> mod (3*x) p) g2_curveB 
    pairMap f (x,y) = (f x, f y)                          -- fucking functor instance on pairs....

--------------------------------------------------------------------------------

hsBegin_xcurve :: Bool -> XCurve -> CodeGenParams -> Code
hsBegin_xcurve is_proj xcurve cgparams = case xcurve of
  Left  c1  -> hsBegin_curve1 is_proj c1  cgparams
  Right c12 -> hsBegin_curve2 is_proj c12 cgparams

hsBegin_curve1 :: Bool -> Curve1 -> CodeGenParams -> Code
hsBegin_curve1 is_proj (Curve1{..}) (CodeGenParams{..}) =
  [ "primeP, primeR, cofactor :: Integer"
  , "primeP = Fp.prime"
  , "primeR = Fr.prime"
  , "cofactor = " ++ show cofactor
  , ""
  , "type Base = Fp"
  , "pattern MkBase fptr = MkFp fptr"
  , ""
  , "-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@"
  , "curveA, curveB :: Integer"
  , "curveA = " ++ show curveA
  , "curveB = " ++ show curveB
  , ""
  , "-- | generator of the r-sized subgroup G1"
  , "genG1 :: " ++ typeName
  , "genG1 = mkPoint (x, y" ++ (if is_proj then ", Fp.one" else "") ++ ") where"
  , "  x = " ++ show (fst subgroupGen)
  , "  y = " ++ show (snd subgroupGen)
  ]

hsBegin_curve2 :: Bool -> Curve12 -> CodeGenParams -> Code
hsBegin_curve2 is_proj (Curve12 (Curve1{..}) (Curve2{..})) (CodeGenParams{..}) =
  [ "primeR, cofactor :: Integer"
  , "primeR = Fr.prime"
  , "cofactor = " ++ show g2_cofactor
  , ""
  , "type Base = Fp2"
  , "pattern MkBase fptr = MkFp2 fptr"
  , ""
  , "-- | parameters A and B of the curve equation @y^2 = x^3 + A*x + B@"
  , "curveA, curveB :: Fp2"
  , "curveA = Fp2.pack " ++ show g2_curveA
  , "curveB = Fp2.pack " ++ show g2_curveB
  , ""
  , "-- | generator of the r-sized subgroup G1"
  , "genG2 :: " ++ typeName
  , "genG2 = mkPoint (x, y" ++ (if is_proj then ", Fp2.one" else "") ++ ") where"
  , "  x = Fp2.pack " ++ show (fst g2_subgroupGen)
  , "  y = Fp2.pack " ++ show (snd g2_subgroupGen)
  ]

hsExportParams :: XCurve -> Code
hsExportParams ei = case ei of
  Left _ ->
    [ "    -- * Parameters"
    , "  , primeP , primeR , cofactor , curveA , curveB"
    , "  , genG1 , infinity"
    ]
  Right _ ->
    [ "    -- * Parameters"
    , "  , primeR , cofactor , curveA , curveB"
    , "  , genG2 , infinity"
    ]

--------------------------------------------------------------------------------

hsSage :: XCurve -> CodeGenParams -> Code
hsSage (Left  curve1 ) = hsSage_curve1 curve1
hsSage (Right curve12) = hsSage_curve2 curve12

hsSage_curve1 :: Curve1 -> CodeGenParams -> Code
hsSage_curve1 curve1 params = 
  [ "-- | Sage setup code to experiment with this curve"
  , "sageSetup :: [String]"
  , "sageSetup = "
  ] ++ 
  (hsQuoteListOfStrings (curveSageSetup curve1)) ++
  [ ""
  , "-- | Prints the Sage code"
  , "printSageSetup :: IO ()"
  , "printSageSetup = mapM_ putStrLn sageSetup"
  ]

hsSage_curve2 :: Curve12 -> CodeGenParams -> Code
hsSage_curve2 curve12 params = 
  [ "-- | Sage setup code to experiment with this curve"
  , "sageSetup :: [String]"
  , "sageSetup = [ \"# Sage for G2: TODO\" ]"
  , ""
  , "-- | Prints the Sage code"
  , "printSageSetup :: IO ()"
  , "printSageSetup = mapM_ putStrLn sageSetup"
  ]

--------------------------------------------------------------------------------
