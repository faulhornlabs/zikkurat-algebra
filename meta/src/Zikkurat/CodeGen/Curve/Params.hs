
module Zikkurat.CodeGen.Curve.Params where

--------------------------------------------------------------------------------

import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data Curve = Curve
  { curveName     :: String                     -- ^ name of the curve
  , curveFp       :: Integer                    -- ^ the prime @p@ of the base field
  , curveFr       :: Integer                    -- ^ the prime @q@ of the scalar field
  , curveA        :: Integer                    -- ^ the A in @y^2 = x^3 + A*x + B@
  , curveB        :: Integer                    -- ^ the B in @y^2 = x^3 + A*x + B@
  , cofactor      :: Integer                    -- ^ the cofactor of the subgroup of size @r@
  , subgroupGen   :: (Integer,Integer)          -- ^ a generator g=(x,y) of the subgroup
  , glvBetaLambda :: Maybe (Integer,Integer)    -- ^ beta and lambda for the GLV trick
  }
  deriving Show

--------------------------------------------------------------------------------

data CodeGenParams = CodeGenParams
  { prefix         :: String       -- ^ prefix for C names (what we are generating)
  , prefix_affine  :: String       -- ^ prefix for C names
  , prefix_proj    :: String       -- ^ prefix for C names
  , prefix_p       :: String       -- ^ prefix for C names for Fp
  , prefix_r       :: String       -- ^ prefix for C names for Fq
  , nlimbs_p       :: Int          -- ^ number of 64-bit limbs in p
  , nlimbs_r       :: Int          -- ^ number of 64-bit limbs in r
  , hs_path_p      :: Path         -- ^ path of the Haskell module for Fp
  , hs_path_r      :: Path         -- ^ path of the Haskell module for Fr
  , hs_path_big_p  :: Path         -- ^ path fot the Fp-sized BigInt module
  , c_path         :: Path         -- ^ path of the C file (what we are generating)
  , c_path_affine  :: Path         -- ^ path of the C file
  , c_path_proj    :: Path         -- ^ path of the C file
  , hs_path        :: Path         -- ^ path of the Haskell module (what we are generating)
  , hs_path_affine :: Path         -- ^ path of the Haskell module
  , hs_path_proj   :: Path         -- ^ path of the Haskell module
  , c_basename_p   :: String       -- ^ name of the @.c@ / @.h@ file for Fr (without extension)
  , c_basename_r   :: String       -- ^ name of the @.c@ / @.h@ file for Fr (without extension)
  , typeName       :: String       -- ^ the name of the haskell type for curve points
  }
  deriving Show

--------------------------------------------------------------------------------

bn128_curve :: Curve
bn128_curve = Curve
  { curveName     = "BN128"
  , curveFp       = 21888242871839275222246405745257275088696311157297823662689037894645226208583 
  , curveFr       = 21888242871839275222246405745257275088548364400416034343698204186575808495617
  , curveA        = 0 
  , curveB        = 3
  , cofactor      = 1
  , subgroupGen   = (1,2)
  , glvBetaLambda = Just
      ( 2203960485148121921418603742825762020974279258880205651966
      , 4407920970296243842393367215006156084916469457145843978461 
      )
  }

--------------------------------------------------------------------------------

bls12_381_curve :: Curve
bls12_381_curve = Curve
  { curveName     = "BLS12-381"
  , curveFp       = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787
  , curveFr       = 52435875175126190479447740508185965837690552500527637822603658699938581184513
  , curveA        = 0
  , curveB        = 4 
  , cofactor      = 76329603384216526031706109802092473003
  , subgroupGen   = 
      ( 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507
      , 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569 
      )
  , glvBetaLambda = Just
      ( 4002409555221667392624310435006688643935503118305586438271171395842971157480381377015405980053539358417135540939436 
      , 228988810152649578064853576960394133503
      )
  }

--------------------------------------------------------------------------------
