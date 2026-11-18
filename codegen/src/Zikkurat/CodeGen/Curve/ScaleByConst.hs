
-- | Scale an Fp element by a constant value (eg. A or B parameter of the curve)

{-# LANGUAGE RecordWildCards #-}
module Zikkurat.CodeGen.Curve.ScaleByConst where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Text.Printf

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc

import Zikkurat.CodeGen.Curve.Params
import Zikkurat.CodeGen.Curve.Shared

--------------------------------------------------------------------------------

-- eg. 
--
-- > scale_by_constant curve cgenparams "A" (-3)
-- > scale_by_constant curve cgenparams "B" 4
-- > scale_by_constant curve cgenparams "B" 0xb441071b12f4a0366fb552f8e21ed4ac36b06aceeb354224863e60f20219fc56
--
scale_by_constant ::  Curve1 -> CodeGenParams -> String -> Integer -> Code
scale_by_constant (Curve1{..}) (CodeGenParams{..}) name kst = text where

  text =  fst3 impl   
       ++ def         ++ snd3 impl ++ [ "}" , "" ] 
       ++ def_inplace ++ thd3 impl ++ [ "}"      ]

  def = 
    [ "// scale a field element by " ++ name ++ " = " ++ show curveA
    , "void " ++ prefix ++ "scale_by_" ++ name ++ "(const uint64_t *src, uint64_t *tgt ) {"
    ]

  def_inplace = 
    [ "void " ++ prefix ++ "scale_by_" ++ name ++ "_inplace( uint64_t *tgt ) {"
    ]

  impl = case kst of

    0 -> 
      ( []
      , [ "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
        ]
      , [ "  memset( tgt, 0, " ++ show (8*nlimbs_p) ++ " );"
        ]
      )
  
    1 -> 
      ( []
      , [ "  if (tgt != src) { memcpy( tgt, src, " ++ show (8*nlimbs_p) ++ " ); }"
        ]
      , [ "  // no-op"
        ]
      )
  
    2 -> 
      ( []
      , [ "  " ++ prefix_p ++ "add( src, src, tgt );"
        ]
      , [ "  " ++ prefix_p ++ "add_inplace( tgt, tgt );"
        ]
      )
  
    3 -> 
      ( []
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( src, src, tmp );"
        , "  " ++ prefix_p ++ "add( src, tmp, tgt );"
        ]
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( tgt, tgt, tmp );"
        , "  " ++ prefix_p ++ "add_inplace( tgt, tmp );"
        ]
      )

    4 -> 
      ( []
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( src, src, tmp );"
        , "  " ++ prefix_p ++ "add( tmp, tmp, tgt );"
        ]
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( tgt, tgt, tmp );"
        , "  " ++ prefix_p ++ "add( tmp, tmp, tgt );"
        ]
      )
  
    -- for P256 and Tom256 we have A == -3
    -3 -> 
      ( []
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( src, src, tmp );"
        , "  " ++ prefix_p ++ "add( src, tmp, tgt );"
        , "  " ++ prefix_p ++ "neg_inplace( tgt );"
        ]
      , [ "  uint64_t tmp[" ++ show nlimbs_p ++ "];"
        , "  " ++ prefix_p ++ "add( tgt, tgt, tmp );"
        , "  " ++ prefix_p ++ "add_inplace( tgt, tmp );"
        , "  " ++ prefix_p ++ "neg_inplace( tgt );"
        ]
      )
  
    _ | kst < 0  -> error "scaling by negative constant not implemented"

    _ | kst < 2^64 ->
          ( []
          , [ "  " ++ prefix_p ++ "scl( " ++ show kst ++ ", src, tgt );"
            ]
          , [ "  " ++ prefix_p ++ "scl_inplace( tgt, " ++ printf "0x%16xll" kst ++ " );"
            ]
          )

    _ | otherwise -> 
          let cname = prefix ++ "const_" ++ name
          in  ( [ 
                ]
              , [ "  " ++ prefix_p ++ "mul( src, " ++ cname ++ ", tgt );"
                ]
              , [ "  " ++ prefix_p ++ "mul_inplace( tgt, " ++ cname ++ " );"
                ]
              )
         
--------------------------------------------------------------------------------
