-- | Convenient reexport of (monomorphic) types for Tom256

module ZK.Algebra.Curves.Tom256 where

--------------------------------------------------------------------------------

import qualified ZK.Algebra.Curves.Tom256.Fp.Mont
import qualified ZK.Algebra.Curves.Tom256.Fr.Mont
import qualified ZK.Algebra.Curves.Tom256.Fr.Std
import qualified ZK.Algebra.Curves.Tom256.G1.Affine
import qualified ZK.Algebra.Curves.Tom256.G1.Proj
import qualified ZK.Algebra.Curves.Tom256.Poly
import qualified ZK.Algebra.Curves.Tom256.Array

--------------------------------------------------------------------------------

type Fp = ZK.Algebra.Curves.Tom256.Fp.Mont.Fp
type Fr = ZK.Algebra.Curves.Tom256.Fr.Mont.Fr
type StdFr = ZK.Algebra.Curves.Tom256.Fr.Std.Fr
type GAffine = ZK.Algebra.Curves.Tom256.G1.Affine.G1
type GProj = ZK.Algebra.Curves.Tom256.G1.Proj.G1
type Poly = ZK.Algebra.Curves.Tom256.Poly.Poly

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

