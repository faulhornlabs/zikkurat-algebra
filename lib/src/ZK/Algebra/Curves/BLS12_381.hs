-- | Convenient reexport of (monomorphic) types for BLS12_381
-- 
-- Elliptic curve `BLS12-381`
-- 
-- see also:
-- 
--   - <https://zodl.com/new-snark-curve/>
--   - <https://std.neuromancer.sk/bls/BLS12-381>
--   - <https://hackmd.io/@benjaminion/bls12-381>
--   - <https://ecdsa.com/curves/bls12-381>

module ZK.Algebra.Curves.BLS12_381 where

--------------------------------------------------------------------------------

import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fp12.Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont
import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std
import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine
import qualified ZK.Algebra.Curves.BLS12_381.G2.Affine
import qualified ZK.Algebra.Curves.BLS12_381.G1.Proj
import qualified ZK.Algebra.Curves.BLS12_381.G2.Proj
import qualified ZK.Algebra.Curves.BLS12_381.Poly
import qualified ZK.Algebra.Curves.BLS12_381.Array
import qualified ZK.Algebra.Curves.BLS12_381.Pairing

--------------------------------------------------------------------------------

type Fp = ZK.Algebra.Curves.BLS12_381.Fp.Mont.Fp
type Fp2 = ZK.Algebra.Curves.BLS12_381.Fp2.Mont.Fp2
type Fp12 = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.Fp12
type Fr = ZK.Algebra.Curves.BLS12_381.Fr.Mont.Fr
type StdFr = ZK.Algebra.Curves.BLS12_381.Fr.Std.Fr
type G1Affine = ZK.Algebra.Curves.BLS12_381.G1.Affine.G1
type G2Affine = ZK.Algebra.Curves.BLS12_381.G2.Affine.G2
type G1Proj = ZK.Algebra.Curves.BLS12_381.G1.Proj.G1
type G2Proj = ZK.Algebra.Curves.BLS12_381.G2.Proj.G2
type Poly = ZK.Algebra.Curves.BLS12_381.Poly.Poly

--------------------------------------------------------------------------------

pairing = ZK.Algebra.Curves.BLS12_381.Pairing.pairing

--------------------------------------------------------------------------------

