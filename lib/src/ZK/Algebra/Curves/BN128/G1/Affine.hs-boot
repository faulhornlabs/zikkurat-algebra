{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module ZK.Algebra.Curves.BN128.G1.Affine where

import Data.Word
import Foreign.ForeignPtr

import ZK.Algebra.Curves.BN128.Fp.Mont ( Fp(..) )
import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr(..) )
import qualified ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as F
import qualified ZK.Algebra.Class.Curve as C
import qualified ZK.Algebra.Class.Misc  as M
import           ZK.Algebra.Class.FFT

-- | An elliptic curve point, in affine coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

instance   Eq          G1
instance   Show        G1
instance L.Flat        G1
instance C.StrictEq    G1
instance M.Rnd         G1
instance C.Group       G1
instance C.Curve       G1
instance C.AffineCurve G1

