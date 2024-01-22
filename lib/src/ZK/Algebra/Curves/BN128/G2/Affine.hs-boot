{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module ZK.Algebra.Curves.BN128.G2.Affine where

import Data.Word
import Foreign.ForeignPtr

import ZK.Algebra.Curves.BN128.Fp.Mont ( Fp(..) )
import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C

-- | An elliptic curve point, in affine coordinates
newtype G2 = MkG2 (ForeignPtr Word64)

instance   Eq          G2
instance   Show        G2
instance L.Flat        G2
instance C.StrictEq    G2
instance F.Rnd         G2
instance C.Group       G2
instance C.Curve       G2
instance C.AffineCurve G2

