module ZK.Algebra.BigInt.Types where

--------------------------------------------------------------------------------

import Data.Word
import Foreign.ForeignPtr

--------------------------------------------------------------------------------

newtype BigInt128  = MkBigInt128  (ForeignPtr Word64)
newtype BigInt192  = MkBigInt192  (ForeignPtr Word64)
newtype BigInt256  = MkBigInt256  (ForeignPtr Word64)
newtype BigInt320  = MkBigInt320  (ForeignPtr Word64)
newtype BigInt384  = MkBigInt384  (ForeignPtr Word64)
newtype BigInt448  = MkBigInt448  (ForeignPtr Word64)
newtype BigInt512  = MkBigInt512  (ForeignPtr Word64)
newtype BigInt576  = MkBigInt576  (ForeignPtr Word64)
newtype BigInt640  = MkBigInt640  (ForeignPtr Word64)
newtype BigInt704  = MkBigInt704  (ForeignPtr Word64)
newtype BigInt768  = MkBigInt768  (ForeignPtr Word64)
newtype BigInt832  = MkBigInt832  (ForeignPtr Word64)
newtype BigInt896  = MkBigInt896  (ForeignPtr Word64)
newtype BigInt960  = MkBigInt960  (ForeignPtr Word64)
newtype BigInt1024 = MkBigInt1024 (ForeignPtr Word64)

--------------------------------------------------------------------------------
