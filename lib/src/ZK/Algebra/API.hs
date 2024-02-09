
-- | Re-export all the @ZK.Algebra.Class.*@ modules for convencience 
--
-- Note: this /does not/ reexport "ZK.Algebra.Class.Pairing", because
-- that reuses the names @Fp@, @Fr@, @G1@ etc.
--
-- This way you can use this module for writing both monomorphic and
-- polymorphic (over the set of elliptic curves) code.
--

module ZK.Algebra.API 
  ( module ZK.Algebra.Class.Field
  , module ZK.Algebra.Class.Curve
  , module ZK.Algebra.Class.FFT
  , module ZK.Algebra.Class.Poly
  , module ZK.Algebra.Class.Vector
  , module ZK.Algebra.Class.Flat
  , module ZK.Algebra.Class.Misc
  )
  where

--------------------------------------------------------------------------------

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.FFT
import ZK.Algebra.Class.Poly
import ZK.Algebra.Class.Vector
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.Misc

--------------------------------------------------------------------------------
