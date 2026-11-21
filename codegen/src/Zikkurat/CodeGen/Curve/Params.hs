
-- | Curve parameters

{-# LANGUAGE RecordWildCards #-}
module Zikkurat.CodeGen.Curve.Params where

--------------------------------------------------------------------------------

import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

type XCurve = Either Curve1 Curve12

extractCurve1 :: XCurve -> Curve1
extractCurve1 ei = case ei of
  Left  c1             -> c1
  Right (Curve12 c1 _) -> c1

extractCurve2 :: XCurve -> Maybe Curve2
extractCurve2 ei = case ei of
  Left  _              -> Nothing
  Right (Curve12 _ c2) -> Just c2

isCurveAZero :: XCurve -> Bool
isCurveAZero ei = case ei of
  Left  c1             -> curveA    c1 == 0
  Right (Curve12 _ c2) -> g2_curveA c2 == (0,0)

isCurveBZero :: XCurve -> Bool
isCurveBZero ei = case ei of
  Left  c1             -> curveB    c1 == 0
  Right (Curve12 _ c2) -> g2_curveB c2 == (0,0)

xcurveBCoeff :: XCurve -> Integer
xcurveBCoeff (Left  curve1 ) = curveB          curve1
xcurveBCoeff (Right curve12) = curveB (_curve1 curve12)

xcurveIsBZero :: XCurve -> Bool
xcurveIsBZero xcurve = xcurveBCoeff xcurve == 0

--------------------------------------------------------------------------------

data Curve12 = Curve12 
  { _curve1 :: Curve1     -- ^ the original curve over Fp
  , _curve2 :: Curve2     -- ^ the twisted curve over Fp2
  }
  deriving Show

-- | The standard curve over @F_p@
data Curve1 = Curve1
  { curveName      :: String                     -- ^ name of the curve
  , curveURLs      :: [String]                   -- ^ internet links
  , curveFp        :: Integer                    -- ^ the prime @p@ of the base field
  , curveFr        :: Integer                    -- ^ the prime @q@ of the scalar field
  , curveA         :: Integer                    -- ^ the A in @y^2 = x^3 + A*x + B@
  , curveB         :: Integer                    -- ^ the B in @y^2 = x^3 + A*x + B@
  , cardinality    :: Integer                    -- ^ cardinality of the curve
  , subgroupOrder' :: Integer                    -- ^ size of the subgroup = @r@
  , cofactor'      :: Integer                    -- ^ the cofactor
  , subgroupGen    :: (Integer,Integer)          -- ^ a generator g=(x,y) of the subgroup
  , glvBetaLambda  :: Maybe (Integer,Integer)    -- ^ beta and lambda for the GLV trick
  }
  deriving Show

subgroupOrder :: Curve1 -> Integer
subgroupOrder curve 
  | one /= two  = error "subgroupOrder: incompatible reference values"
  | otherwise   = one
  where
    one = curveFr curve 
    two = subgroupOrder' curve

-- | the cofactor of the subgroup of size @r@
cofactor :: Curve1 -> Integer
cofactor curve 
  | r /= 0     = error "cofactor: cardinality is not divisble by subgroup order"
  | q /= h     = error "cofactor: computed cofactor does not match the reference value"
  | otherwise  = h
  where
    (q,r) = divMod (cardinality curve) (subgroupOrder curve)
    h     = cofactor' curve

-- | Ghetto encoding of elements of Fp2
type I2 = (Integer,Integer)

----------------------------------------

-- | Twisted curve of @F_{p^2}@
data Curve2 = Curve2
  { g2_curveA        :: I2                   -- ^ the A in @y^2 = x^3 + A*x + B@
  , g2_curveB        :: I2                   -- ^ the B in @y^2 = x^3 + A*x + B@
  , g2_cardinality   :: Integer              -- ^ cardinality of the curve
  , g2_subgroupOrder :: Integer              -- ^ size of the subgroup = @r@
  , g2_cofactor'     :: Integer              -- ^ size of the subgroup = @r@
  , g2_subgroupGen   :: (I2,I2)              -- ^ a generator g=(x,y) of the subgroup
  }
  deriving Show

-- | the cofactor of the subgroup of size @r@
g2_cofactor :: Curve2 -> Integer
g2_cofactor curve 
  | r /= 0     = error "g2_cofactor: cardinality is not divisble by subgroup order"
  | q /= h     = error "g2_cofactor: computed cofactor does not match the reference value"
  | otherwise  = h
  where
    (q,r) = divMod (g2_cardinality curve) (g2_subgroupOrder curve)
    h     = g2_cofactor' curve

----------------------------------------

xcurveCardinality :: XCurve -> Integer
xcurveCardinality xcurve = case xcurve of
  Left  curve1  ->    cardinality   curve1
  Right curve12 -> g2_cardinality (_curve2 curve12)
  
xcurveSubgroupOrder :: XCurve -> Integer
xcurveSubgroupOrder xcurve = case xcurve of
  Left  curve1  ->    subgroupOrder   curve1
  Right curve12 -> g2_subgroupOrder (_curve2 curve12)

xcurveCofactor :: XCurve -> Integer
xcurveCofactor xcurve = case xcurve of
  Left  curve1  ->    cofactor   curve1
  Right curve12 -> g2_cofactor (_curve2 curve12)

--------------------------------------------------------------------------------

data CodeGenParams = CodeGenParams
  { prefix         :: String       -- ^ prefix for C names (what we are generating)
  , prefix_affine  :: String       -- ^ prefix for C names
  , prefix_proj    :: String       -- ^ prefix for C names
  , prefix_jac     :: String       -- ^ prefix for C names
  , prefix_p       :: String       -- ^ prefix for C names for Fp / Fp2 (the base field)
  , prefix_r       :: String       -- ^ prefix for C names for Fr
  , point_repr     :: String       -- one of "affine", "proj" or "jac"
  , nlimbs_p       :: Int          -- ^ number of 64-bit limbs in p
  , nlimbs_r       :: Int          -- ^ number of 64-bit limbs in r
  , hs_path_p      :: Path         -- ^ path of the Haskell module for Fp
  , hs_path_fp2    :: Path         -- ^ path of the Haskell module for Fp2
  , hs_path_r      :: Path         -- ^ path of the Haskell module for Fr (Montgomery repr)
  , hs_path_r_std  :: Path         -- ^ path of the Haskell module for Fr (standard repr)
  , hs_path_big_p  :: Path         -- ^ path for the Fp-sized BigInt module
  , c_path         :: Path         -- ^ path of the C file (what we are generating)
  , c_path_affine  :: Path         -- ^ path of the C file
  , c_path_proj    :: Path         -- ^ path of the C file
  , c_path_jac     :: Path         -- ^ path of the C file
  , hs_path        :: Path         -- ^ path of the Haskell module (what we are generating)
  , hs_path_affine :: Path         -- ^ path of the Haskell module
  , hs_path_proj   :: Path         -- ^ path of the Haskell module
  , hs_path_jac    :: Path         -- ^ path of the Haskell module
  , c_basename_p   :: String       -- ^ name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r   :: String       -- ^ name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName       :: String       -- ^ the name of the haskell type for curve points
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Produce Sage code to experiment with the curve
curveSageSetup :: Curve1 -> [String]
curveSageSetup curve1@(Curve1{..}) = 
  [ "# " ++ curveName ++ " elliptic curve"
  , "p  = " ++ show curveFp
  , "r  = " ++ show curveFr
  , "h  = " ++ show (cofactor curve1)
  , "Fp = GF(p)"
  , "Fr = GF(r)"
  , "A  = Fp(" ++ show curveA ++ ")"
  , "B  = Fp(" ++ show curveB ++ ")"
  , "E  = EllipticCurve(Fp,[A,B])"
  , "gx = Fp(" ++ show (fst subgroupGen) ++ ")"
  , "gy = Fp(" ++ show (snd subgroupGen) ++ ")"
  , "gen = E(gx,gy)  # subgroup generator"
  , "print(\"scalar field check: \", gen.additive_order() == r )"
  , "print(\"cofactor check:     \", E.cardinality() == r*h )" 
  ] ++
  (case glvBetaLambda of
     Nothing -> []
     Just (beta,lambda) -> 
       [ ""
       , "# GLV beta and gamma parameters"
       , "beta = Fp(" ++ show beta   ++ ")"
       , "lam  = " ++ show lambda 
       , "pt   = 1234567 * gen;"
       , "pt2  = E( beta*pt[0] , pt[1], pt[2] )"
       , "print(\"beta check:   \", beta^3 == 1 )"
       , "print(\"lambda check: \", Fr(lam)^3 == 1 )"
       , "print(\"GLV check:    \", lam * pt == pt2 )"
       ]
  )

printSageSetup :: Curve1 -> IO ()
printSageSetup curve = mapM_ putStrLn $ curveSageSetup curve

hsQuoteListOfStrings :: [String] -> [String]
hsQuoteListOfStrings xs = zipWith f chs xs ++ [close] where
  chs   = '[' : repeat ','
  close = "  ]"
  f ch str = "  " ++ [ch] ++ " \"" ++ hsEscapeStr str ++ "\""

hsEscapeStr :: String -> String
hsEscapeStr = concatMap g where
  g '"'  = "\\\""
  g '\\' = "\\\\"
  g '\n' = "\\n"
  g ch   = [ch] 

--------------------------------------------------------------------------------

bn128_curve12 :: Curve12
bn128_curve12 = Curve12 bn128_curve1 bn128_curve2

bn128_curve1 :: Curve1
bn128_curve1 = Curve1
  { curveName      = "BN254 [aka. alt-bn128]"
  , curveURLs      = [ "https://ecdsa.com/curves/bn254"
                     , "https://hackmd.io/@jpw/bn254"
                     ]
  , curveFp        = 21888242871839275222246405745257275088696311157297823662689037894645226208583 
  , curveFr        = 21888242871839275222246405745257275088548364400416034343698204186575808495617
  , curveA         = 0 
  , curveB         = 3
  , cardinality    = 21888242871839275222246405745257275088548364400416034343698204186575808495617
  , subgroupOrder' = 21888242871839275222246405745257275088548364400416034343698204186575808495617
  , cofactor'      = 1
  , subgroupGen    = (1,2)
  , glvBetaLambda  = Just
      ( 2203960485148121921418603742825762020974279258880205651966
      , 4407920970296243842393367215006156084916469457145843978461 
      )
  }

bn128_curve2 :: Curve2
bn128_curve2 = Curve2
  { g2_curveA        = (0 ,0 ) 
  , g2_curveB        = (b1,bu) 
  , g2_cardinality   = 479095176016622842441988045216678740799252316531100822436447802254070093686356349204969212544220033486413271283566945264650845755880805213916963058350733
  , g2_subgroupOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617
  , g2_cofactor'     = 21888242871839275222246405745257275088844257914179612981679871602714643921549
  , g2_subgroupGen   = ( (gen_x1,gen_xu) , (gen_y1,gen_yu) )
  }
  where
    b1 = 19485874751759354771024239261021720505790618469301721065564631296452457478373 
    bu = 266929791119991161246907387137283842545076965332900288569378510910307636690 
    gen_x1 = 0x1adcd0ed10df9cb87040f46655e3808f98aa68a570acf5b0bde23fab1f149701 
    gen_xu = 0x09e847e9f05a6082c3cd2a1d0a3a82e6fbfbe620f7f31269fa15d21c1c13b23b 
    gen_y1 = 0x056c01168a5319461f7ca7aa19d4fcfd1c7cdf52dbfc4cbee6f915250b7f6fc8 
    gen_yu = 0x0efe500a2d02dd77f5f401329f30895df553b878fc3c0dadaaa86456a623235c 

--------------------------------------------------------------------------------

bls12_381_curve12 :: Curve12
bls12_381_curve12 = Curve12 bls12_381_curve1 bls12_381_curve2

bls12_381_curve1 :: Curve1
bls12_381_curve1 = Curve1
  { curveName      = "BLS12-381"
  , curveURLs      = [ "https://zodl.com/new-snark-curve/"
                     , "https://std.neuromancer.sk/bls/BLS12-381"
                     , "https://hackmd.io/@benjaminion/bls12-381"
                     , "https://ecdsa.com/curves/bls12-381"
                     ]
  , curveFp        = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787
  , curveFr        = 52435875175126190479447740508185965837690552500527637822603658699938581184513
  , curveA         = 0
  , curveB         = 4 
  , cardinality    = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129030796414117214202539
  , subgroupOrder' = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  , cofactor'      = 76329603384216526031706109802092473003
  , subgroupGen    = 
      ( 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
      , 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569 
      )
  , glvBetaLambda = Just
      ( 4002409555221667392624310435006688643935503118305586438271171395842971157480381377015405980053539358417135540939436 
      , 228988810152649578064853576960394133503
      )
  }

bls12_381_curve2 :: Curve2
bls12_381_curve2 = Curve2
  { g2_curveA        = (0 , 0) 
  , g2_curveB        = (4 , 4)   -- 4(1+i)
  , g2_cardinality   = 16019282247729705411943748644318972617695120099330552659862384536985976748491357143400656079302193429974954385540174732940659106207100323726025938325193045129788127168347624263893040187112659960846674086295148469572963088890738917
  , g2_subgroupOrder = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
  , g2_cofactor'     = 305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041616661285803823378372096355777062779109
  , g2_subgroupGen   = ( (gen_x1,gen_xu) , (gen_y1,gen_yu) )
  }
  where
    gen_xu = 3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758 -- *u 
    gen_x1 = 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160 
    gen_yu = 927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582  -- *u 
    gen_y1 = 1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905

--------------------------------------------------------------------------------

-- https://std.neuromancer.sk/nist/P-256
p256_curve1 :: Curve1
p256_curve1 = Curve1
  { curveName      = "P-256"
  , curveURLs      = [ "https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-186.pdf"
                     , "https://std.neuromancer.sk/nist/P-256"
                     , "https://ecdsa.com/curves/p-256"
                     ]
  , curveFp        = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  , curveFr        = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  , curveA         = -3 -- 0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc  -- = -3
  , curveB         = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
  , cardinality    = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  , subgroupOrder' = 0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
  , cofactor'      = 1
  , subgroupGen    = ( 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
                     , 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5 
                     )
  , glvBetaLambda  = Nothing
  }

----------------------------------------

-- https://std.neuromancer.sk/other/Tom-256
tom256_curve1 :: Curve1
tom256_curve1 = Curve1
  { curveName      = "Tom-256"
  , curveURLs      = [ "https://eprint.iacr.org/2021/1183"
                     , "https://std.neuromancer.sk/other/Tom-256" 
                     ]
  , curveFp        = 0xffffffff0000000100000000000000017e72b42b30e7317793135661b1c4b117
  , curveFr        = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  , curveA         = -3  -- 0xffffffff0000000100000000000000017e72b42b30e7317793135661b1c4b114
  , curveB         = 0xb441071b12f4a0366fb552f8e21ed4ac36b06aceeb354224863e60f20219fc56
  , cardinality    = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  , subgroupOrder' = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  , cofactor'      = 1
  , subgroupGen    = (0x03, 0x5a6dd32df58708e64e97345cbe66600decd9d538a351bb3c30b4954925b1f02d)
  , glvBetaLambda  = Nothing
  }

--------------------------------------------------------------------------------
