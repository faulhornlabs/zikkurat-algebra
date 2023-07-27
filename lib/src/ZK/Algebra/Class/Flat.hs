
-- | Objects having flat representation in memory
--
-- Examples are: bigints, field elements, elliptic curve points
--

{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, FlexibleContexts #-}
module ZK.Algebra.Class.Flat where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word
import Data.Proxy

import Control.Monad

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal

import System.IO.Unsafe

--------------------------------------------------------------------------------

-- | This is kind of similar to @Storable@, but we expect the object
-- to be stored in some piece of continuous foreign memory.
class Flat a where
  -- | The size of the object in bytes
  sizeInBytes  :: Proxy a -> Int
  -- | The size of the object in 64-bit words 
  sizeInQWords :: Proxy a -> Int
  -- | Access to the raw data
  withFlat :: a -> (Ptr Word64 -> IO b) -> IO b
  -- | Create a new instance by copying the data from memory
  makeFlat :: Ptr Word64 -> IO a

makeFlatGeneric :: (ForeignPtr Word64 -> a) -> Int -> Ptr Word64 -> IO a
makeFlatGeneric wrap nwords srcPtr = do
  fptr <- mallocForeignPtrBytes (8*nwords)
  withForeignPtr fptr $ \tgtPtr -> copyBytes tgtPtr srcPtr (8*nwords)
  return (wrap fptr)

peekFlat :: forall a. Flat a => a -> IO [Word64]
peekFlat what = withFlat what $ \ptr -> peekArray (sizeInQWords pxy) ptr where
  pxy = Proxy @a

--------------------------------------------------------------------------------

-- | Something which is a newtype containing a FlatArray
class WrappedArray a where
  type Element a :: *
  wrapArray   :: FlatArray (Element a) -> a
  unwrapArray :: a -> FlatArray (Element a)

--------------------------------------------------------------------------------

-- | A flat array of flat objects, represented as a continuous segment of 
-- foreign memory (not managed by the Haskell runtime). 
--
-- Note: the @Int@ means the number of objects in the array.
data FlatArray a 
  = MkFlatArray !Int !(ForeignPtr Word64)
  deriving Show
  
withFlatArray :: FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO b
withFlatArray (MkFlatArray n fptr) action = do
  withForeignPtr fptr $ \ptr -> action n ptr

-- TODO:
-- parallelWithFlatArray :: Int -> FlatArray a -> (Int -> Ptr Word64 -> IO b) -> IO [b]

--------------------------------------------------------------------------------
-- * Pack \/ unpack flat arrays

-- | Create a flat array from elements. This is intended mostly for experimenting
-- and testing, as this is not a very efficient way of doing things.
--
{-# NOINLINE packFlatArray #-}
packFlatArray :: Flat a => Array Int a -> FlatArray a
packFlatArray arr = unsafePerformIO (packFlatArrayIO arr)

-- | Create a flat array from elements from a list.
packFlatArrayFromList :: Flat a => [a] -> FlatArray a
packFlatArrayFromList list = packFlatArrayFromList' (length list) list

-- | Create a flat array from elements from a list with a given size.
{-# NOINLINE packFlatArrayFromList #-}
packFlatArrayFromList' :: Flat a => Int -> [a] -> FlatArray a
packFlatArrayFromList' len list = unsafePerformIO (packFlatArrayFromListIO len list)

{-# NOINLINE unpackFlatArrayToList #-}
unpackFlatArrayToList :: Flat a => FlatArray a -> [a]
unpackFlatArrayToList flatArr = unsafePerformIO (unpackFlatArrayToListIO flatArr)

{-# NOINLINE unpackFlatArray #-}
unpackFlatArray :: Flat a => FlatArray a -> Array Int a
unpackFlatArray flatArr@(MkFlatArray len _) = unsafePerformIO $ do
  list <- unpackFlatArrayToListIO flatArr
  return $ listArray (0,len-1) list

--------------------------------------------------------------------------------
-- * Pack \/ unpack flat arrays in IO

{-# NOINLINE packFlatArrayIO #-}
packFlatArrayIO :: forall a. Flat a => Array Int a -> IO (FlatArray a)
packFlatArrayIO arr = do
  let (a,b) = bounds arr
  let n  = b-a+1
  packFlatArrayFromListIO n (elems arr)

{-# NOINLINE packFlatArrayFromListIO #-}
packFlatArrayFromListIO :: forall a. Flat a => Int -> [a] -> IO (FlatArray a)
packFlatArrayFromListIO n list = do
  let sz = sizeInBytes (Proxy @a) 
  fptr <- mallocForeignPtrBytes (n*sz)
  withForeignPtr fptr $ \arrPtr -> do
    forM_ (zip [0..n-1] list) $ \(j,x) -> do
      let tgt = plusPtr arrPtr (j*sz)
      withFlat x $ \src -> copyBytes tgt src sz
  return $ MkFlatArray n fptr    

{-# NOINLINE unpackFlatArrayToListIO #-}
unpackFlatArrayToListIO :: forall a. Flat a => FlatArray a -> IO [a]
unpackFlatArrayToListIO (MkFlatArray len fptr) = do
  let sz = sizeInBytes (Proxy @a) 
  withForeignPtr fptr $ \arrPtr -> do
    forM [0..len-1] $ \j -> do
      let src = plusPtr arrPtr (j*sz)
      makeFlat src

--------------------------------------------------------------------------------
