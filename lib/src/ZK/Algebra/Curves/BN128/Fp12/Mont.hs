
-- | Algebraic field extension `BN128/Fp12`
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}
module ZK.Algebra.Curves.BN128.Fp12.Mont
  ( Fp12(..)
    -- * Conversion
  , pack , unpack
  , packList , unpackList
  , packListPrime , unpackListPrime
    -- * Field elements
  , zero , one , two     -- , primGen
    -- * Predicates
  , isValid , isZero , isOne , isEqual
    -- * Field operations
  , neg , add , sub
  , sqr , mul
  , inv , div , divBy2 , batchInv
    -- * Exponentiation
  , pow , pow_
    -- * Relation to the base and prime fields
  , embedBase , embedPrime
  , scaleBase , scalePrime
    -- * Frobenius automorphism
  , frob
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

import ZK.Algebra.Curves.BN128.Fp6.Mont( Fp6(..) )
import ZK.Algebra.Curves.BN128.Fp.Mont( Fp(..) )
import qualified ZK.Algebra.Curves.BN128.Fp6.Mont as Base
import qualified ZK.Algebra.Curves.BN128.Fp.Mont as Prime

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import qualified ZK.Algebra.Class.Misc  as M
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  


newtype Fp12 = MkFp12 (ForeignPtr Word64)

zero, one, two :: Fp12
zero = embedPrime 0
one  = embedPrime 1
two  = embedPrime 2

primGen :: Fp12
primGen = error "primGen/BN128/Fp12: not implemented"

instance Eq Fp12 where
  (==) = isEqual

instance Num Fp12 where
  fromInteger = embedPrime . fromInteger
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
  sizeInBytes  _pxy = 384
  sizeInQWords _pxy = 48
  withFlat (MkFp12 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFp12 48

rnd :: IO Fp12
rnd = do
  x1 <- Base.rnd
  x2 <- Base.rnd
  return $ pack (x1, x2)

instance M.Rnd Fp12 where
  rndIO = rnd

instance C.Ring Fp12 where
  ringNamePxy _ = "BN128/Fp12"
  ringSizePxy _ = C.ringSizePxy (Proxy @Fp6) ^ 2
  isZero = ZK.Algebra.Curves.BN128.Fp12.Mont.isZero
  isOne  = ZK.Algebra.Curves.BN128.Fp12.Mont.isOne
  zero   = ZK.Algebra.Curves.BN128.Fp12.Mont.zero
  one    = ZK.Algebra.Curves.BN128.Fp12.Mont.one
  square = ZK.Algebra.Curves.BN128.Fp12.Mont.sqr
  power  = ZK.Algebra.Curves.BN128.Fp12.Mont.pow

instance C.Field Fp12 where
  charPxy    _ = C.charPxy (Proxy @Fp6)
  dimPxy     _ = C.dimPxy  (Proxy @Fp6) * 2
  primGenPxy _ = primGen
  batchInverse = batchInv
  frobenius    = frob
  halve        = divBy2

instance C.ExtField Fp12 where
  type ExtBase Fp12 = Fp6
  extDeg _ = 2
  definingPolyCoeffs = error "definingPolyCoeffs: not yet implemented"
  embedExtBase     = embedBase
  projectToExtBase = error "projectToExtBase: not yet implemented"
  scaleExtBase     = scaleBase
  extPack          = packList
  extUnpack        = unpackList

instance C.ExtField' Fp12 where
  type PrimeBase Fp12 = Fp
  embedPrimeField     = embedPrime
  projectToPrimeField = error "projectToPrimeField: not yet implemented"
  scalePrimeField     = scalePrime
  primePack           = packListPrime
  primeUnpack         = unpackListPrime

instance C.QuadraticExt Fp12 where
  quadraticUnpack = unpack
  quadraticPack   = pack

----------------------------------------

foreign import ccall unsafe "bn128_Fp12_mont_set_const" c_bn128_Fp12_mont_set_const :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bn128_Fp6_mont_copy"      c_bn128_Fp6_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_Fp_mont_copy"      c_bn128_Fp_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bn128_Fp12_mont_from_base_field"  c_bn128_Fp12_mont_from_base_field  :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_Fp12_mont_from_prime_field" c_bn128_Fp12_mont_from_prime_field :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bn128_Fp12_mont_scale_by_base_field"  c_bn128_Fp12_mont_scale_by_base_field  :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bn128_Fp12_mont_scale_by_prime_field" c_bn128_Fp12_mont_scale_by_prime_field :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

mallocForeignTuple :: IO (ForeignPtr Word64, ForeignPtr Word64)
mallocForeignTuple = do
  fptr1 <- mallocForeignPtrArray 24
  fptr2 <- mallocForeignPtrArray 24
  return (fptr1, fptr2)

withForeignTuple :: (ForeignPtr Word64, ForeignPtr Word64) -> ((Ptr Word64, Ptr Word64) -> IO a) -> IO a
withForeignTuple (fptr1, fptr2) action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  action (ptr1, ptr2)

----------------

mallocForeignList :: IO [ForeignPtr Word64]
mallocForeignList = do
  fptr1 <- mallocForeignPtrArray 24
  fptr2 <- mallocForeignPtrArray 24
  return [fptr1, fptr2]

withForeignList :: [ForeignPtr Word64] -> ([Ptr Word64] -> IO a) -> IO a
withForeignList [fptr1, fptr2] action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  action [ptr1, ptr2]

----------------

mallocForeignListPrime :: IO [ForeignPtr Word64]
mallocForeignListPrime = do
  fptr1 <- mallocForeignPtrArray 4
  fptr2 <- mallocForeignPtrArray 4
  fptr3 <- mallocForeignPtrArray 4
  fptr4 <- mallocForeignPtrArray 4
  fptr5 <- mallocForeignPtrArray 4
  fptr6 <- mallocForeignPtrArray 4
  fptr7 <- mallocForeignPtrArray 4
  fptr8 <- mallocForeignPtrArray 4
  fptr9 <- mallocForeignPtrArray 4
  fptr10 <- mallocForeignPtrArray 4
  fptr11 <- mallocForeignPtrArray 4
  fptr12 <- mallocForeignPtrArray 4
  return [fptr1, fptr2, fptr3, fptr4, fptr5, fptr6, fptr7, fptr8, fptr9, fptr10, fptr11, fptr12]

withForeignListPrime :: [ForeignPtr Word64] -> ([Ptr Word64] -> IO a) -> IO a
withForeignListPrime [fptr1, fptr2, fptr3, fptr4, fptr5, fptr6, fptr7, fptr8, fptr9, fptr10, fptr11, fptr12] action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  withForeignPtr fptr3 $ \ptr3 -> do
  withForeignPtr fptr4 $ \ptr4 -> do
  withForeignPtr fptr5 $ \ptr5 -> do
  withForeignPtr fptr6 $ \ptr6 -> do
  withForeignPtr fptr7 $ \ptr7 -> do
  withForeignPtr fptr8 $ \ptr8 -> do
  withForeignPtr fptr9 $ \ptr9 -> do
  withForeignPtr fptr10 $ \ptr10 -> do
  withForeignPtr fptr11 $ \ptr11 -> do
  withForeignPtr fptr12 $ \ptr12 -> do
  action [ptr1, ptr2, ptr3, ptr4, ptr5, ptr6, ptr7, ptr8, ptr9, ptr10, ptr11, ptr12]

----------------------------------------

{-# NOINLINE embedBase#-}
embedBase :: Fp6 -> Fp12
embedBase (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_from_base_field ptr1 ptr2
  return (MkFp12 fptr2)

{-# NOINLINE embedPrime#-}
embedPrime :: Fp -> Fp12
embedPrime (MkFp fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_from_prime_field ptr1 ptr2
  return (MkFp12 fptr2)

----------------------------------------

{-# NOINLINE scaleBase#-}
scaleBase :: Fp6 -> Fp12 -> Fp12
scaleBase (MkFp6 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_scale_by_base_field ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

{-# NOINLINE scalePrime#-}
scalePrime :: Fp -> Fp12 -> Fp12
scalePrime (MkFp fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_scale_by_prime_field ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

----------------------------------------

{-# NOINLINE unpack #-}
unpack :: Fp12 -> (Fp6, Fp6)
unpack (MkFp12 fsrc) = unsafePerformIO $ do
  fptrs@(ftgt1, ftgt2) <- mallocForeignTuple
  withForeignPtr fsrc $ \src -> do
    withForeignTuple fptrs $ \(tgt1, tgt2) -> do
      c_bn128_Fp6_mont_copy (plusPtr src 0) tgt1
      c_bn128_Fp6_mont_copy (plusPtr src 192) tgt2
  return (MkFp6 ftgt1, MkFp6 ftgt2)

{-# NOINLINE pack #-}
pack :: (Fp6, Fp6) -> Fp12
pack (MkFp6 fsrc1, MkFp6 fsrc2) = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 48
  withForeignTuple (fsrc1, fsrc2) $ \(src1, src2) -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bn128_Fp6_mont_copy src1 (plusPtr tgt 0)
      c_bn128_Fp6_mont_copy src2 (plusPtr tgt 192)
  return (MkFp12 ftgt)

----------------------------------------

{-# NOINLINE unpackList #-}
unpackList :: Fp12 -> [Fp6]
unpackList (MkFp12 fsrc) = unsafePerformIO $ do
  fptrs@[ftgt1, ftgt2] <- mallocForeignList
  withForeignPtr fsrc $ \src -> do
    withForeignList fptrs $ \[tgt1, tgt2] -> do
      c_bn128_Fp6_mont_copy (plusPtr src 0) tgt1
      c_bn128_Fp6_mont_copy (plusPtr src 192) tgt2
  return [MkFp6 ftgt1, MkFp6 ftgt2]

{-# NOINLINE packList #-}
packList :: [Fp6] -> Fp12
packList [MkFp6 fsrc1, MkFp6 fsrc2] = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 48
  withForeignList [fsrc1, fsrc2] $ \[src1, src2] -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bn128_Fp6_mont_copy src1 (plusPtr tgt 0)
      c_bn128_Fp6_mont_copy src2 (plusPtr tgt 192)
  return (MkFp12 ftgt)
packList _ = error "expecting a list of 2 Fp6 elements"

----------------------------------------

{-# NOINLINE unpackListPrime #-}
unpackListPrime :: Fp12 -> [Fp]
unpackListPrime (MkFp12 fsrc) = unsafePerformIO $ do
  fptrs@[ftgt1, ftgt2, ftgt3, ftgt4, ftgt5, ftgt6, ftgt7, ftgt8, ftgt9, ftgt10, ftgt11, ftgt12] <- mallocForeignListPrime
  withForeignPtr fsrc $ \src -> do
    withForeignListPrime fptrs $ \[tgt1, tgt2, tgt3, tgt4, tgt5, tgt6, tgt7, tgt8, tgt9, tgt10, tgt11, tgt12] -> do
      c_bn128_Fp_mont_copy (plusPtr src 0) tgt1
      c_bn128_Fp_mont_copy (plusPtr src 32) tgt2
      c_bn128_Fp_mont_copy (plusPtr src 64) tgt3
      c_bn128_Fp_mont_copy (plusPtr src 96) tgt4
      c_bn128_Fp_mont_copy (plusPtr src 128) tgt5
      c_bn128_Fp_mont_copy (plusPtr src 160) tgt6
      c_bn128_Fp_mont_copy (plusPtr src 192) tgt7
      c_bn128_Fp_mont_copy (plusPtr src 224) tgt8
      c_bn128_Fp_mont_copy (plusPtr src 256) tgt9
      c_bn128_Fp_mont_copy (plusPtr src 288) tgt10
      c_bn128_Fp_mont_copy (plusPtr src 320) tgt11
      c_bn128_Fp_mont_copy (plusPtr src 352) tgt12
  return [MkFp ftgt1, MkFp ftgt2, MkFp ftgt3, MkFp ftgt4, MkFp ftgt5, MkFp ftgt6, MkFp ftgt7, MkFp ftgt8, MkFp ftgt9, MkFp ftgt10, MkFp ftgt11, MkFp ftgt12]

{-# NOINLINE packListPrime #-}
packListPrime :: [Fp] -> Fp12
packListPrime [MkFp fsrc1, MkFp fsrc2, MkFp fsrc3, MkFp fsrc4, MkFp fsrc5, MkFp fsrc6, MkFp fsrc7, MkFp fsrc8, MkFp fsrc9, MkFp fsrc10, MkFp fsrc11, MkFp fsrc12] = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 48
  withForeignListPrime [fsrc1, fsrc2, fsrc3, fsrc4, fsrc5, fsrc6, fsrc7, fsrc8, fsrc9, fsrc10, fsrc11, fsrc12] $ \[src1, src2, src3, src4, src5, src6, src7, src8, src9, src10, src11, src12] -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bn128_Fp_mont_copy src1 (plusPtr tgt 0)
      c_bn128_Fp_mont_copy src2 (plusPtr tgt 32)
      c_bn128_Fp_mont_copy src3 (plusPtr tgt 64)
      c_bn128_Fp_mont_copy src4 (plusPtr tgt 96)
      c_bn128_Fp_mont_copy src5 (plusPtr tgt 128)
      c_bn128_Fp_mont_copy src6 (plusPtr tgt 160)
      c_bn128_Fp_mont_copy src7 (plusPtr tgt 192)
      c_bn128_Fp_mont_copy src8 (plusPtr tgt 224)
      c_bn128_Fp_mont_copy src9 (plusPtr tgt 256)
      c_bn128_Fp_mont_copy src10 (plusPtr tgt 288)
      c_bn128_Fp_mont_copy src11 (plusPtr tgt 320)
      c_bn128_Fp_mont_copy src12 (plusPtr tgt 352)
  return (MkFp12 ftgt)
packListPrime _ = error "expecting a list of 12 Fp elements"

----------------------------------------

{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fp12 -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fp12] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 48 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bn128_Fp12_mont_pow_gen" c_bn128_Fp12_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

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
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFp12 fptr3)

{-# NOINLINE pow #-}
powBigInt256 :: Fp12 -> BigInt256 -> Fp12
powBigInt256 (MkFp12 fptr1) (MkBigInt256 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_pow_gen ptr1 ptr2 ptr3 48
  return (MkFp12 fptr3)

----------------------------------------

foreign import ccall unsafe "bn128_Fp12_mont_batch_inv" c_bn128_Fp12_mont_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fp12 -> FlatArray Fp12
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*48)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


foreign import ccall unsafe "bn128_Fp12_mont_is_zero" c_bn128_Fp12_mont_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fp12 -> Bool
isZero (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp12_mont_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp12_mont_is_one" c_bn128_Fp12_mont_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fp12 -> Bool
isOne (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp12_mont_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp12_mont_is_equal" c_bn128_Fp12_mont_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fp12 -> Fp12 -> Bool
isEqual (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp12_mont_is_valid" c_bn128_Fp12_mont_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fp12 -> Bool
isValid (MkFp12 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bn128_Fp12_mont_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bn128_Fp12_mont_neg" c_bn128_Fp12_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fp12 -> Fp12
neg (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_neg ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bn128_Fp12_mont_add" c_bn128_Fp12_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fp12 -> Fp12 -> Fp12
add (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_add ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bn128_Fp12_mont_sub" c_bn128_Fp12_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fp12 -> Fp12 -> Fp12
sub (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_sub ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bn128_Fp12_mont_sqr" c_bn128_Fp12_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fp12 -> Fp12
sqr (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_sqr ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bn128_Fp12_mont_mul" c_bn128_Fp12_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fp12 -> Fp12 -> Fp12
mul (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_mul ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bn128_Fp12_mont_inv" c_bn128_Fp12_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fp12 -> Fp12
inv (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_inv ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bn128_Fp12_mont_div" c_bn128_Fp12_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fp12 -> Fp12 -> Fp12
div (MkFp12 fptr1) (MkFp12 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bn128_Fp12_mont_div ptr1 ptr2 ptr3
  return (MkFp12 fptr3)

foreign import ccall unsafe "bn128_Fp12_mont_div_by_2" c_bn128_Fp12_mont_div_by_2 :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE divBy2 #-}
divBy2 :: Fp12 -> Fp12
divBy2 (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_div_by_2 ptr1 ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bn128_Fp12_mont_pow_uint64" c_bn128_Fp12_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fp12 -> Word64 -> Fp12
pow_ (MkFp12 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_pow_uint64 ptr1 x ptr2
  return (MkFp12 fptr2)

foreign import ccall unsafe "bn128_Fp12_mont_frobenius" c_bn128_Fp12_mont_frobenius :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE frob #-}
frob :: Fp12 -> Fp12
frob (MkFp12 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 48
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bn128_Fp12_mont_frobenius ptr1 ptr2
  return (MkFp12 fptr2)
