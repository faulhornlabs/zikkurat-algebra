
-- | Algebraic field extension `BN128/Fp2`
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}
module ZK.Algebra.Curves.BN128.Fp2.Mont
  ( Fp2(..)
    -- * Conversion
  , pack , unpack
    -- * Field elements
  , embedBase
  , zero , one , two     -- , primGen
    -- * Predicates
  , isValid , isZero , isOne , isEqual
    -- * Field operations
  , neg , add , sub
  , sqr , mul
  , inv , div , batchInv
    -- * Exponentiation
  , pow , pow_
    -- * Random
  , rnd
    -- * Export to C
  , exportToCDef , exportListToCDef
  )
  where

--------------------------------------------------------------------------------

import Prelude  hiding (div)
import GHC.Real hiding (div)

import Data.Bits
import Data.Word
import Data.Proxy

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.ForeignPtr

import System.Random
import System.IO.Unsafe

import ZK.Algebra.BigInt.BigInt256 (BigInt256(..) )

import ZK.Algebra.Curves.BN128.Fp.Mont( Fp(..) )
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as Base

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  


newtype Fp2 = MkFp2 (ForeignPtr Word64)

zero, one, two :: Fp2
zero = embedBase 0
one  = embedBase 1
two  = embedBase 2

primGen :: Fp2
primGen = error "primGen/BN128/Fp2: not implemented"

instance Eq Fp2 where
  (==) = isEqual

instance Num Fp2 where
  fromInteger = embedBase . fromInteger
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Fractional Fp2 where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  (/)   = div

instance Show Fp2 where
  show = show . unpack    -- temp. TODO: proper show

instance L.Flat Fp2 where
  sizeInBytes  _pxy = 64
  sizeInQWords _pxy = 8
  withFlat (MkFp2 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFp2 8

rnd :: IO Fp2
rnd = do
  x1 <- Base.rnd
  x2 <- Base.rnd
  return $ pack (x1, x2)

instance C.Rnd Fp2 where
  rndIO = rnd

instance C.Ring Fp2 where
  ringNamePxy _ = "BN128/Fp2"
  ringSizePxy _ = C.ringSizePxy (Proxy @Fp) ^ 2
  isZero = ZK.Algebra.Curves.BN128.Fp2.Mont.isZero
  isOne  = ZK.Algebra.Curves.BN128.Fp2.Mont.isOne
  zero   = ZK.Algebra.Curves.BN128.Fp2.Mont.zero
  one    = ZK.Algebra.Curves.BN128.Fp2.Mont.one
  square = ZK.Algebra.Curves.BN128.Fp2.Mont.sqr
  power  = ZK.Algebra.Curves.BN128.Fp2.Mont.pow

instance C.Field Fp2 where
  charPxy    _ = C.charPxy (Proxy @Fp)
  dimPxy     _ = C.dimPxy  (Proxy @Fp) * 2
  primGenPxy _ = primGen
  batchInverse = batchInv

instance C.ExtField Fp2 where
  type ExtBase Fp2 = Fp
  extDeg _     = 2
  embedExtBase = embedBase
  definingPolyCoeffs = error "definingPolyCoeffs: not yet implemented"
  projectToExtBase   = error "projectToExtBase: not yet implemented"

instance C.QuadraticExt Fp2 where
  quadraticUnpack = unpack
  quadraticPack   = pack

----------------------------------------

foreign import ccall unsafe "bn128_Fp2_mont_set_const" c_bn128_Fp2_mont_set_const :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_Fp_mont_copy" c_bn128_Fp_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

mallocForeignTuple :: IO (ForeignPtr Word64, ForeignPtr Word64)
mallocForeignTuple = do
  fptr1 <- mallocForeignPtrArray 4
  fptr2 <- mallocForeignPtrArray 4
  return (fptr1, fptr2)

withForeignTuple :: (ForeignPtr Word64, ForeignPtr Word64) -> ((Ptr Word64, Ptr Word64) -> IO a) -> IO a
withForeignTuple (fptr1, fptr2) action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  action (ptr1, ptr2)

----------------------------------------

{-# NOINLINE embedBase#-}
embedBase :: Fp -> Fp2
embedBase (MkFp fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_set_const ptr1 ptr2
  return (MkFp2 fptr2)

{-# NOINLINE unpack #-}
unpack :: Fp2 -> (Fp, Fp)
unpack (MkFp2 fsrc) = unsafePerformIO $ do
  fptrs@(ftgt1, ftgt2) <- mallocForeignTuple
  withForeignPtr fsrc $ \src -> do
    withForeignTuple fptrs $ \(tgt1, tgt2) -> do
      c_bn128_Fp_mont_copy (plusPtr src 0) tgt1
      c_bn128_Fp_mont_copy (plusPtr src 32) tgt2
  return (MkFp ftgt1, MkFp ftgt2)

{-# NOINLINE pack #-}
pack :: (Fp, Fp) -> Fp2
pack (MkFp fsrc1, MkFp fsrc2) = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 8
  withForeignTuple (fsrc1, fsrc2) $ \(src1, src2) -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bn128_Fp_mont_copy src1 (plusPtr tgt 0)
      c_bn128_Fp_mont_copy src2 (plusPtr tgt 32)
  return (MkFp2 ftgt)


{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fp2 -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fp2] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 8 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bn128_Fp2_mont_pow_gen" c_bn128_Fp2_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

pow :: Fp2 -> Integer -> Fp2
pow x e
  | e == 0      = if isZero x then zero else one
  | e >  0      =      powNonNeg x         e  
  | otherwise   = inv (powNonNeg x (negate e))

withInteger :: Integer -> ((Int, Ptr Word64) -> IO a) -> IO a
withInteger !input action = do
  let (n,ws) = toWord64sLE_ input
  allocaArray n $ \ptr -> do
    pokeArray ptr ws
    action (n,ptr)

{-# NOINLINE powNonNeg #-}
powNonNeg :: Fp2 -> Integer -> Fp2
powNonNeg (MkFp2 fptr1) expo = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFp2 fptr3)

{-# NOINLINE pow #-}
powBigInt256 :: Fp2 -> BigInt256 -> Fp2
powBigInt256 (MkFp2 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_pow_gen ptr1 ptr2 ptr3 8
  return (MkFp2 fptr3)

----------------------------------------

foreign import ccall unsafe "bn128_Fp2_mont_batch_inv" c_bn128_Fp2_mont_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fp2 -> FlatArray Fp2
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*8)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


foreign import ccall unsafe "bn128_Fp2_mont_is_zero" c_bn128_Fp2_mont_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fp2 -> Bool
isZero (MkFp2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp2_mont_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp2_mont_is_one" c_bn128_Fp2_mont_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fp2 -> Bool
isOne (MkFp2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp2_mont_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp2_mont_is_equal" c_bn128_Fp2_mont_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fp2 -> Fp2 -> Bool
isEqual (MkFp2 fptr1) (MkFp2 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp2_mont_is_valid" c_bn128_Fp2_mont_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fp2 -> Bool
isValid (MkFp2 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp2_mont_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp2_mont_neg" c_bn128_Fp2_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fp2 -> Fp2
neg (MkFp2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_neg ptr1 ptr2
  return (MkFp2 fptr2)

foreign import ccall unsafe "bn128_Fp2_mont_add" c_bn128_Fp2_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fp2 -> Fp2 -> Fp2
add (MkFp2 fptr1) (MkFp2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_add ptr1 ptr2 ptr3
  return (MkFp2 fptr3)

foreign import ccall unsafe "bn128_Fp2_mont_sub" c_bn128_Fp2_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fp2 -> Fp2 -> Fp2
sub (MkFp2 fptr1) (MkFp2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_sub ptr1 ptr2 ptr3
  return (MkFp2 fptr3)

foreign import ccall unsafe "bn128_Fp2_mont_sqr" c_bn128_Fp2_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fp2 -> Fp2
sqr (MkFp2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_sqr ptr1 ptr2
  return (MkFp2 fptr2)

foreign import ccall unsafe "bn128_Fp2_mont_mul" c_bn128_Fp2_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fp2 -> Fp2 -> Fp2
mul (MkFp2 fptr1) (MkFp2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_mul ptr1 ptr2 ptr3
  return (MkFp2 fptr3)

foreign import ccall unsafe "bn128_Fp2_mont_inv" c_bn128_Fp2_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fp2 -> Fp2
inv (MkFp2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_inv ptr1 ptr2
  return (MkFp2 fptr2)

foreign import ccall unsafe "bn128_Fp2_mont_div" c_bn128_Fp2_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fp2 -> Fp2 -> Fp2
div (MkFp2 fptr1) (MkFp2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 8
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp2_mont_div ptr1 ptr2 ptr3
  return (MkFp2 fptr3)

foreign import ccall unsafe "bn128_Fp2_mont_pow_uint64" c_bn128_Fp2_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fp2 -> Word64 -> Fp2
pow_ (MkFp2 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 8
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp2_mont_pow_uint64 ptr1 x ptr2
  return (MkFp2 fptr2)
