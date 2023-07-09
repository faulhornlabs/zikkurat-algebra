
-- | Objects having flat representation in memory
--
-- Examples are: bigints, field elements, elliptic curve points
--

{-# LANGUAGE ScopedTypeVariables #-}
module ZK.Algebra.Class.Flat where

--------------------------------------------------------------------------------

import Data.Word
import Data.Proxy

import Foreign.Ptr
import Foreign.ForeignPtr

--------------------------------------------------------------------------------

-- | This is similar to @Storable@, but we only need a size
class Flat a where
  -- | The size of the object in bytes
  sizeInBytes  :: Proxy a -> Int
  -- | The size of the object in 64-bit words 
  sizeInQWords :: Proxy a -> Int

--------------------------------------------------------------------------------

-- | A flat array of flat objects, represented as a continuous portion of 
-- memory (not managed by the Haskell runtime).
data FlatArray a 
  = MkFlatArray !Int !(ForeignPtr Word64)
  deriving Show

withFlatArray :: FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO b
withFlatArray (MkFlatArray n fptr) action = do
  withForeignPtr fptr $ \ptr -> action n ptr

-- TODO:
-- parallelWithFlatArray :: Int -> FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO [b]

--------------------------------------------------------------------------------
