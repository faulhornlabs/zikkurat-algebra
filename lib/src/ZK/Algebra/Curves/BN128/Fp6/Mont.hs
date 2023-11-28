
-- | Algebraic field extension `BN128/Fp6`
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}
module ZK.Algebra.Curves.BN128.Fp6.Mont
  ( Fp6(..)
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

import ZK.Algebra.Curves.BN128.Fp2.Mont( Fp2(..) )
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont as Base

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  


newtype Fp6 = MkFp6 (ForeignPtr Word64)

zero, one, two :: Fp6
zero = embedBase 0
one  = embedBase 1
two  = embedBase 2

primGen :: Fp6
primGen = error "primGen/BN128/Fp6: not implemented"

instance Eq Fp6 where
  (==) = isEqual

instance Num Fp6 where
  fromInteger = embedBase . fromInteger
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Fractional Fp6 where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  (/)   = div

instance Show Fp6 where
  show = show . unpack    -- temp. TODO: proper show

instance L.Flat Fp6 where
  sizeInBytes  _pxy = 192
  sizeInQWords _pxy = 24
  withFlat (MkFp6 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFp6 24

rnd :: IO Fp6
rnd = do
  x1 <- Base.rnd
  x2 <- Base.rnd
  x3 <- Base.rnd
  return $ pack (x1, x2, x3)

instance C.Rnd Fp6 where
  rndIO = rnd

instance C.Ring Fp6 where
  ringNamePxy _ = "BN128/Fp6"
  ringSizePxy _ = C.ringSizePxy (Proxy @Fp2) ^ 3
  isZero = ZK.Algebra.Curves.BN128.Fp6.Mont.isZero
  isOne  = ZK.Algebra.Curves.BN128.Fp6.Mont.isOne
  zero   = ZK.Algebra.Curves.BN128.Fp6.Mont.zero
  one    = ZK.Algebra.Curves.BN128.Fp6.Mont.one
  square = ZK.Algebra.Curves.BN128.Fp6.Mont.sqr
  power  = ZK.Algebra.Curves.BN128.Fp6.Mont.pow

instance C.Field Fp6 where
  charPxy    _ = C.charPxy (Proxy @Fp2)
  dimPxy     _ = C.dimPxy  (Proxy @Fp2) * 3
  primGenPxy _ = primGen
  batchInverse = batchInv

instance C.ExtField Fp6 where
  type ExtBase Fp6 = Fp2
  extDeg _     = 3
  embedExtBase = embedBase
  definingPolyCoeffs = error "definingPolyCoeffs: not yet implemented"
  projectToExtBase   = error "projectToExtBase: not yet implemented"

instance C.CubicExt Fp6 where
  cubicUnpack = unpack
  cubicPack   = pack

----------------------------------------

foreign import ccall unsafe "bn128_Fp6_mont_set_const" c_bn128_Fp6_mont_set_const :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_Fp2_mont_copy" c_bn128_Fp2_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

mallocForeignTuple :: IO (ForeignPtr Word64, ForeignPtr Word64, ForeignPtr Word64)
mallocForeignTuple = do
  fptr1 <- mallocForeignPtrArray 8
  fptr2 <- mallocForeignPtrArray 8
  fptr3 <- mallocForeignPtrArray 8
  return (fptr1, fptr2, fptr3)

withForeignTuple :: (ForeignPtr Word64, ForeignPtr Word64, ForeignPtr Word64) -> ((Ptr Word64, Ptr Word64, Ptr Word64) -> IO a) -> IO a
withForeignTuple (fptr1, fptr2, fptr3) action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  withForeignPtr fptr3 $ \ptr3 -> do
  action (ptr1, ptr2, ptr3)

----------------------------------------

{-# NOINLINE embedBase#-}
embedBase :: Fp2 -> Fp6
embedBase (MkFp2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_set_const ptr1 ptr2
  return (MkFp6 fptr2)

{-# NOINLINE unpack #-}
unpack :: Fp6 -> (Fp2, Fp2, Fp2)
unpack (MkFp6 fsrc) = unsafePerformIO $ do
  fptrs@(ftgt1, ftgt2, ftgt3) <- mallocForeignTuple
  withForeignPtr fsrc $ \src -> do
    withForeignTuple fptrs $ \(tgt1, tgt2, tgt3) -> do
      c_bn128_Fp2_mont_copy (plusPtr src 0) tgt1
      c_bn128_Fp2_mont_copy (plusPtr src 64) tgt2
      c_bn128_Fp2_mont_copy (plusPtr src 128) tgt3
  return (MkFp2 ftgt1, MkFp2 ftgt2, MkFp2 ftgt3)

{-# NOINLINE pack #-}
pack :: (Fp2, Fp2, Fp2) -> Fp6
pack (MkFp2 fsrc1, MkFp2 fsrc2, MkFp2 fsrc3) = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 24
  withForeignTuple (fsrc1, fsrc2, fsrc3) $ \(src1, src2, src3) -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bn128_Fp2_mont_copy src1 (plusPtr tgt 0)
      c_bn128_Fp2_mont_copy src2 (plusPtr tgt 64)
      c_bn128_Fp2_mont_copy src3 (plusPtr tgt 128)
  return (MkFp6 ftgt)


{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fp6 -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fp6] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 24 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bn128_Fp6_mont_pow_gen" c_bn128_Fp6_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

pow :: Fp6 -> Integer -> Fp6
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
powNonNeg :: Fp6 -> Integer -> Fp6
powNonNeg (MkFp6 fptr1) expo = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFp6 fptr3)

{-# NOINLINE pow #-}
powBigInt256 :: Fp6 -> BigInt256 -> Fp6
powBigInt256 (MkFp6 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_pow_gen ptr1 ptr2 ptr3 24
  return (MkFp6 fptr3)

----------------------------------------

foreign import ccall unsafe "bn128_Fp6_mont_batch_inv" c_bn128_Fp6_mont_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fp6 -> FlatArray Fp6
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*24)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


foreign import ccall unsafe "bn128_Fp6_mont_is_zero" c_bn128_Fp6_mont_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fp6 -> Bool
isZero (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp6_mont_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp6_mont_is_one" c_bn128_Fp6_mont_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fp6 -> Bool
isOne (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp6_mont_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp6_mont_is_equal" c_bn128_Fp6_mont_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fp6 -> Fp6 -> Bool
isEqual (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp6_mont_is_valid" c_bn128_Fp6_mont_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fp6 -> Bool
isValid (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp6_mont_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp6_mont_neg" c_bn128_Fp6_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fp6 -> Fp6
neg (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_neg ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bn128_Fp6_mont_add" c_bn128_Fp6_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fp6 -> Fp6 -> Fp6
add (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_add ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bn128_Fp6_mont_sub" c_bn128_Fp6_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fp6 -> Fp6 -> Fp6
sub (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_sub ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bn128_Fp6_mont_sqr" c_bn128_Fp6_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fp6 -> Fp6
sqr (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_sqr ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bn128_Fp6_mont_mul" c_bn128_Fp6_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fp6 -> Fp6 -> Fp6
mul (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_mul ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bn128_Fp6_mont_inv" c_bn128_Fp6_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fp6 -> Fp6
inv (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_inv ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bn128_Fp6_mont_div" c_bn128_Fp6_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fp6 -> Fp6 -> Fp6
div (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 24
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp6_mont_div ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bn128_Fp6_mont_pow_uint64" c_bn128_Fp6_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fp6 -> Word64 -> Fp6
pow_ (MkFp6 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 24
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp6_mont_pow_uint64 ptr1 x ptr2
  return (MkFp6 fptr2)
