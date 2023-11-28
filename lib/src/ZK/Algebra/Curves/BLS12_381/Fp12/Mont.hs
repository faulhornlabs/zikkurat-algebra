
-- | Algebraic field extension `BLS12_381/Fp12`
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}
module ZK.Algebra.Curves.BLS12_381.Fp12.Mont
  ( Fp12(..)
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

import ZK.Algebra.BigInt.BigInt384 (BigInt384(..) )

import ZK.Algebra.Curves.BLS12_381.Fp6.Mont( Fp6(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Fp6.Mont as Base

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  


newtype Fp12 = MkFp12 (ForeignPtr Word64)

zero, one, two :: Fp12
zero = embedBase 0
one  = embedBase 1
two  = embedBase 2

primGen :: Fp12
primGen = error "primGen/BLS12_381/Fp12: not implemented"

instance Eq Fp12 where
  (==) = isEqual

instance Num Fp12 where
  fromInteger = embedBase . fromInteger
  negate = neg
  (+) = add
  (-) = sub
  (*) = mul
  abs    = id
  signum = \_ -> one

instance Fractional Fp12 where
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  recip = inv
  (/)   = div

instance Show Fp12 where
  show = show . unpack    -- temp. TODO: proper show

instance L.Flat Fp12 where
  sizeInBytes  _pxy = 576
  sizeInQWords _pxy = 72
  withFlat (MkFp12 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFp12 72

rnd :: IO Fp12
rnd = do
  x1 <- Base.rnd
  x2 <- Base.rnd
  return $ pack (x1, x2)

instance C.Rnd Fp12 where
  rndIO = rnd

instance C.Ring Fp12 where
  ringNamePxy _ = "BLS12_381/Fp12"
  ringSizePxy _ = C.ringSizePxy (Proxy @Fp6) ^ 2
  isZero = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.isZero
  isOne  = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.isOne
  zero   = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.zero
  one    = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.one
  square = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.sqr
  power  = ZK.Algebra.Curves.BLS12_381.Fp12.Mont.pow

instance C.Field Fp12 where
  charPxy    _ = C.charPxy (Proxy @Fp6)
  dimPxy     _ = C.dimPxy  (Proxy @Fp6) * 2
  primGenPxy _ = primGen
  batchInverse = batchInv

instance C.ExtField Fp12 where
  type ExtBase Fp12 = Fp6
  extDeg _     = 2
  embedExtBase = embedBase
  definingPolyCoeffs = error "definingPolyCoeffs: not yet implemented"
  projectToExtBase   = error "projectToExtBase: not yet implemented"

instance C.QuadraticExt Fp12 where
  quadraticUnpack = unpack
  quadraticPack   = pack

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp12_mont_set_const" c_bls12_381_Fp12_mont_set_const :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_Fp6_mont_copy" c_bls12_381_Fp6_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

mallocForeignTuple :: IO (ForeignPtr Word64, ForeignPtr Word64)
mallocForeignTuple = do
  fptr1 <- mallocForeignPtrArray 36
  fptr2 <- mallocForeignPtrArray 36
  return (fptr1, fptr2)

withForeignTuple :: (ForeignPtr Word64, ForeignPtr Word64) -> ((Ptr Word64, Ptr Word64) -> IO a) -> IO a
withForeignTuple (fptr1, fptr2) action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  action (ptr1, ptr2)

----------------------------------------

{-# NOINLINE embedBase#-}
embedBase :: Fp6 -> Fp12
embedBase (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_set_const ptr1 ptr2
  return (MkFp12 fptr2)

{-# NOINLINE unpack #-}
unpack :: Fp12 -> (Fp6, Fp6)
unpack (MkFp12 fsrc) = unsafePerformIO $ do
  fptrs@(ftgt1, ftgt2) <- mallocForeignTuple
  withForeignPtr fsrc $ \src -> do
    withForeignTuple fptrs $ \(tgt1, tgt2) -> do
      c_bls12_381_Fp6_mont_copy (plusPtr src 0) tgt1
      c_bls12_381_Fp6_mont_copy (plusPtr src 288) tgt2
  return (MkFp6 ftgt1, MkFp6 ftgt2)

{-# NOINLINE pack #-}
pack :: (Fp6, Fp6) -> Fp12
pack (MkFp6 fsrc1, MkFp6 fsrc2) = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 72
  withForeignTuple (fsrc1, fsrc2) $ \(src1, src2) -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bls12_381_Fp6_mont_copy src1 (plusPtr tgt 0)
      c_bls12_381_Fp6_mont_copy src2 (plusPtr tgt 288)
  return (MkFp12 ftgt)


{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fp12 -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fp12] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 72 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp12_mont_pow_gen" c_bls12_381_Fp12_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

pow :: Fp12 -> Integer -> Fp12
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
powNonNeg :: Fp12 -> Integer -> Fp12
powNonNeg (MkFp12 fptr1) expo = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFp12 fptr3)

{-# NOINLINE pow #-}
powBigInt384 :: Fp12 -> BigInt384 -> Fp12
powBigInt384 (MkFp12 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_pow_gen ptr1 ptr2 ptr3 72
  return (MkFp12 fptr3)

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp12_mont_batch_inv" c_bls12_381_Fp12_mont_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fp12 -> FlatArray Fp12
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*72)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


foreign import ccall unsafe "bls12_381_Fp12_mont_is_zero" c_bls12_381_Fp12_mont_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fp12 -> Bool
isZero (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp12_mont_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp12_mont_is_one" c_bls12_381_Fp12_mont_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fp12 -> Bool
isOne (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp12_mont_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp12_mont_is_equal" c_bls12_381_Fp12_mont_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fp12 -> Fp12 -> Bool
isEqual (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp12_mont_is_valid" c_bls12_381_Fp12_mont_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fp12 -> Bool
isValid (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp12_mont_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp12_mont_neg" c_bls12_381_Fp12_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fp12 -> Fp12
neg (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_neg ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bls12_381_Fp12_mont_add" c_bls12_381_Fp12_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fp12 -> Fp12 -> Fp12
add (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_add ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bls12_381_Fp12_mont_sub" c_bls12_381_Fp12_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fp12 -> Fp12 -> Fp12
sub (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_sub ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bls12_381_Fp12_mont_sqr" c_bls12_381_Fp12_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fp12 -> Fp12
sqr (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_sqr ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bls12_381_Fp12_mont_mul" c_bls12_381_Fp12_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fp12 -> Fp12 -> Fp12
mul (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_mul ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bls12_381_Fp12_mont_inv" c_bls12_381_Fp12_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fp12 -> Fp12
inv (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_inv ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bls12_381_Fp12_mont_div" c_bls12_381_Fp12_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fp12 -> Fp12 -> Fp12
div (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp12_mont_div ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bls12_381_Fp12_mont_pow_uint64" c_bls12_381_Fp12_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fp12 -> Word64 -> Fp12
pow_ (MkFp12 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 72
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp12_mont_pow_uint64 ptr1 x ptr2
  return (MkFp12 fptr2)
