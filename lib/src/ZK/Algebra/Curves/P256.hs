-- | Convenient reexport of (monomorphic) types for P256

module ZK.Algebra.Curves.P256 where

--------------------------------------------------------------------------------

import qualified ZK.Algebra.Curves.P256.Fp.Mont
import qualified ZK.Algebra.Curves.P256.Fr.Mont
import qualified ZK.Algebra.Curves.P256.Fr.Std
import qualified ZK.Algebra.Curves.P256.G1.Affine
import qualified ZK.Algebra.Curves.P256.G1.Proj
import qualified ZK.Algebra.Curves.P256.Poly
import qualified ZK.Algebra.Curves.P256.Array

--------------------------------------------------------------------------------

type Fp = ZK.Algebra.Curves.P256.Fp.Mont.Fp
type Fr = ZK.Algebra.Curves.P256.Fr.Mont.Fr
type StdFr = ZK.Algebra.Curves.P256.Fr.Std.Fr
type GAffine = ZK.Algebra.Curves.P256.G1.Affine.G1
type GProj = ZK.Algebra.Curves.P256.G1.Proj.G1
type Poly = ZK.Algebra.Curves.P256.Poly.Poly

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

