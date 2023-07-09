{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module ZK.Algebra.Curves.BLS12_381.G1.Affine where

import Data.Word
import Foreign.ForeignPtr

import ZK.Algebra.Curves.BLS12_381.Fp.Mont ( Fp(..) )
import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C

-- | An elliptic curve point, in affine coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

instance   Eq          G1
instance   Show        G1
instance C.StrictEq    G1
instance F.Rnd         G1
instance C.Group       G1
instance C.Curve       G1
instance C.AffineCurve G1

