module ZK.Algebra.Curves.BN128.G1.Affine where

import Data.Word
import Foreign.ForeignPtr

-- | An elliptic curve point, in affine coordinates
newtype G1 = MkG1 (ForeignPtr Word64)

