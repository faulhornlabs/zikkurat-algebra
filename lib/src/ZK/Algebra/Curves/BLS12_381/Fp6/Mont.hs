
-- | Algebraic field extension `BLS12_381/Fp6`
--
-- * NOTE 1: This module is intented to be imported qualified
--
-- * NOTE 2: Generated code, do not edit!

{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NondecreasingIndentation, TypeApplications, TypeFamilies #-}
module ZK.Algebra.Curves.BLS12_381.Fp6.Mont
  ( Fp6(..)
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

import ZK.Algebra.BigInt.BigInt384 (BigInt384(..) )

import ZK.Algebra.Curves.BLS12_381.Fp2.Mont( Fp2(..) )
import ZK.Algebra.Curves.BLS12_381.Fp.Mont( Fp(..) )
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont as Base
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont as Prime

import           ZK.Algebra.Class.Flat  as L
import qualified ZK.Algebra.Class.Field as C
import qualified ZK.Algebra.Class.Misc  as M
import ZK.Algebra.Helpers

--------------------------------------------------------------------------------  


newtype Fp6 = MkFp6 (ForeignPtr Word64)

zero, one, two :: Fp6
zero = embedPrime 0
one  = embedPrime 1
two  = embedPrime 2

primGen :: Fp6
primGen = error "primGen/BLS12_381/Fp6: not implemented"

instance Eq Fp6 where
  (==) = isEqual

instance Num Fp6 where
  fromInteger = embedPrime . fromInteger
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
  sizeInBytes  _pxy = 288
  sizeInQWords _pxy = 36
  withFlat (MkFp6 fptr) = withForeignPtr fptr
  makeFlat = L.makeFlatGeneric MkFp6 36

rnd :: IO Fp6
rnd = do
  x1 <- Base.rnd
  x2 <- Base.rnd
  x3 <- Base.rnd
  return $ pack (x1, x2, x3)

instance M.Rnd Fp6 where
  rndIO = rnd

instance C.Ring Fp6 where
  ringName _ = "BLS12_381/Fp6"
  ringSize _ = C.ringSize (Proxy @Fp2) ^ 3
  isZero = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.isZero
  isOne  = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.isOne
  zero   = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.zero
  one    = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.one
  square = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.sqr
  power  = ZK.Algebra.Curves.BLS12_381.Fp6.Mont.pow

instance C.Field Fp6 where
  characteristics _ = C.characteristics (Proxy @Fp2)
  dimension       _ = C.dimension       (Proxy @Fp2) * 3
  primGenPxy _ = primGen
  batchInverse = batchInv
  frobenius    = frob
  halve        = divBy2

instance C.ExtField Fp6 where
  type ExtBase Fp6 = Fp2
  extDeg _ = 3
  definingPolyCoeffs = error "definingPolyCoeffs: not yet implemented"
  embedExtBase     = embedBase
  projectToExtBase = error "projectToExtBase: not yet implemented"
  scaleExtBase     = scaleBase
  extPack          = packList
  extUnpack        = unpackList

instance C.ExtField' Fp6 where
  type PrimeBase Fp6 = Fp
  embedPrimeField     = embedPrime
  projectToPrimeField = error "projectToPrimeField: not yet implemented"
  scalePrimeField     = scalePrime
  primePack           = packListPrime
  primeUnpack         = unpackListPrime

instance C.CubicExt Fp6 where
  cubicUnpack = unpack
  cubicPack   = pack

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp6_mont_set_const" c_bls12_381_Fp6_mont_set_const :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bls12_381_Fp2_mont_copy"      c_bls12_381_Fp2_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_Fp_mont_copy"      c_bls12_381_Fp_mont_copy      :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bls12_381_Fp6_mont_from_base_field"  c_bls12_381_Fp6_mont_from_base_field  :: Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_Fp6_mont_from_prime_field" c_bls12_381_Fp6_mont_from_prime_field :: Ptr Word64 -> Ptr Word64 -> IO ()

foreign import ccall unsafe "bls12_381_Fp6_mont_scale_by_base_field"  c_bls12_381_Fp6_mont_scale_by_base_field  :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_Fp6_mont_scale_by_prime_field" c_bls12_381_Fp6_mont_scale_by_prime_field :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

----------------------------------------

mallocForeignTuple :: IO (ForeignPtr Word64, ForeignPtr Word64, ForeignPtr Word64)
mallocForeignTuple = do
  fptr1 <- mallocForeignPtrArray 12
  fptr2 <- mallocForeignPtrArray 12
  fptr3 <- mallocForeignPtrArray 12
  return (fptr1, fptr2, fptr3)

withForeignTuple :: (ForeignPtr Word64, ForeignPtr Word64, ForeignPtr Word64) -> ((Ptr Word64, Ptr Word64, Ptr Word64) -> IO a) -> IO a
withForeignTuple (fptr1, fptr2, fptr3) action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  withForeignPtr fptr3 $ \ptr3 -> do
  action (ptr1, ptr2, ptr3)

----------------

mallocForeignList :: IO [ForeignPtr Word64]
mallocForeignList = do
  fptr1 <- mallocForeignPtrArray 12
  fptr2 <- mallocForeignPtrArray 12
  fptr3 <- mallocForeignPtrArray 12
  return [fptr1, fptr2, fptr3]

withForeignList :: [ForeignPtr Word64] -> ([Ptr Word64] -> IO a) -> IO a
withForeignList [fptr1, fptr2, fptr3] action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  withForeignPtr fptr3 $ \ptr3 -> do
  action [ptr1, ptr2, ptr3]

----------------

mallocForeignListPrime :: IO [ForeignPtr Word64]
mallocForeignListPrime = do
  fptr1 <- mallocForeignPtrArray 6
  fptr2 <- mallocForeignPtrArray 6
  fptr3 <- mallocForeignPtrArray 6
  fptr4 <- mallocForeignPtrArray 6
  fptr5 <- mallocForeignPtrArray 6
  fptr6 <- mallocForeignPtrArray 6
  return [fptr1, fptr2, fptr3, fptr4, fptr5, fptr6]

withForeignListPrime :: [ForeignPtr Word64] -> ([Ptr Word64] -> IO a) -> IO a
withForeignListPrime [fptr1, fptr2, fptr3, fptr4, fptr5, fptr6] action = do
  withForeignPtr fptr1 $ \ptr1 -> do
  withForeignPtr fptr2 $ \ptr2 -> do
  withForeignPtr fptr3 $ \ptr3 -> do
  withForeignPtr fptr4 $ \ptr4 -> do
  withForeignPtr fptr5 $ \ptr5 -> do
  withForeignPtr fptr6 $ \ptr6 -> do
  action [ptr1, ptr2, ptr3, ptr4, ptr5, ptr6]

----------------------------------------

{-# NOINLINE embedBase#-}
embedBase :: Fp2 -> Fp6
embedBase (MkFp2 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_from_base_field ptr1 ptr2
  return (MkFp6 fptr2)

{-# NOINLINE embedPrime#-}
embedPrime :: Fp -> Fp6
embedPrime (MkFp fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_from_prime_field ptr1 ptr2
  return (MkFp6 fptr2)

----------------------------------------

{-# NOINLINE scaleBase#-}
scaleBase :: Fp2 -> Fp6 -> Fp6
scaleBase (MkFp2 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_scale_by_base_field ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

{-# NOINLINE scalePrime#-}
scalePrime :: Fp -> Fp6 -> Fp6
scalePrime (MkFp fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_scale_by_prime_field ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

----------------------------------------

{-# NOINLINE unpack #-}
unpack :: Fp6 -> (Fp2, Fp2, Fp2)
unpack (MkFp6 fsrc) = unsafePerformIO $ do
  fptrs@(ftgt1, ftgt2, ftgt3) <- mallocForeignTuple
  withForeignPtr fsrc $ \src -> do
    withForeignTuple fptrs $ \(tgt1, tgt2, tgt3) -> do
      c_bls12_381_Fp2_mont_copy (plusPtr src 0) tgt1
      c_bls12_381_Fp2_mont_copy (plusPtr src 96) tgt2
      c_bls12_381_Fp2_mont_copy (plusPtr src 192) tgt3
  return (MkFp2 ftgt1, MkFp2 ftgt2, MkFp2 ftgt3)

{-# NOINLINE pack #-}
pack :: (Fp2, Fp2, Fp2) -> Fp6
pack (MkFp2 fsrc1, MkFp2 fsrc2, MkFp2 fsrc3) = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 36
  withForeignTuple (fsrc1, fsrc2, fsrc3) $ \(src1, src2, src3) -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bls12_381_Fp2_mont_copy src1 (plusPtr tgt 0)
      c_bls12_381_Fp2_mont_copy src2 (plusPtr tgt 96)
      c_bls12_381_Fp2_mont_copy src3 (plusPtr tgt 192)
  return (MkFp6 ftgt)

----------------------------------------

{-# NOINLINE unpackList #-}
unpackList :: Fp6 -> [Fp2]
unpackList (MkFp6 fsrc) = unsafePerformIO $ do
  fptrs@[ftgt1, ftgt2, ftgt3] <- mallocForeignList
  withForeignPtr fsrc $ \src -> do
    withForeignList fptrs $ \[tgt1, tgt2, tgt3] -> do
      c_bls12_381_Fp2_mont_copy (plusPtr src 0) tgt1
      c_bls12_381_Fp2_mont_copy (plusPtr src 96) tgt2
      c_bls12_381_Fp2_mont_copy (plusPtr src 192) tgt3
  return [MkFp2 ftgt1, MkFp2 ftgt2, MkFp2 ftgt3]

{-# NOINLINE packList #-}
packList :: [Fp2] -> Fp6
packList [MkFp2 fsrc1, MkFp2 fsrc2, MkFp2 fsrc3] = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 36
  withForeignList [fsrc1, fsrc2, fsrc3] $ \[src1, src2, src3] -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bls12_381_Fp2_mont_copy src1 (plusPtr tgt 0)
      c_bls12_381_Fp2_mont_copy src2 (plusPtr tgt 96)
      c_bls12_381_Fp2_mont_copy src3 (plusPtr tgt 192)
  return (MkFp6 ftgt)
packList _ = error "expecting a list of 3 Fp2 elements"

----------------------------------------

{-# NOINLINE unpackListPrime #-}
unpackListPrime :: Fp6 -> [Fp]
unpackListPrime (MkFp6 fsrc) = unsafePerformIO $ do
  fptrs@[ftgt1, ftgt2, ftgt3, ftgt4, ftgt5, ftgt6] <- mallocForeignListPrime
  withForeignPtr fsrc $ \src -> do
    withForeignListPrime fptrs $ \[tgt1, tgt2, tgt3, tgt4, tgt5, tgt6] -> do
      c_bls12_381_Fp_mont_copy (plusPtr src 0) tgt1
      c_bls12_381_Fp_mont_copy (plusPtr src 48) tgt2
      c_bls12_381_Fp_mont_copy (plusPtr src 96) tgt3
      c_bls12_381_Fp_mont_copy (plusPtr src 144) tgt4
      c_bls12_381_Fp_mont_copy (plusPtr src 192) tgt5
      c_bls12_381_Fp_mont_copy (plusPtr src 240) tgt6
  return [MkFp ftgt1, MkFp ftgt2, MkFp ftgt3, MkFp ftgt4, MkFp ftgt5, MkFp ftgt6]

{-# NOINLINE packListPrime #-}
packListPrime :: [Fp] -> Fp6
packListPrime [MkFp fsrc1, MkFp fsrc2, MkFp fsrc3, MkFp fsrc4, MkFp fsrc5, MkFp fsrc6] = unsafePerformIO $ do
  ftgt <- mallocForeignPtrArray 36
  withForeignListPrime [fsrc1, fsrc2, fsrc3, fsrc4, fsrc5, fsrc6] $ \[src1, src2, src3, src4, src5, src6] -> do
    withForeignPtr ftgt $ \tgt -> do
      c_bls12_381_Fp_mont_copy src1 (plusPtr tgt 0)
      c_bls12_381_Fp_mont_copy src2 (plusPtr tgt 48)
      c_bls12_381_Fp_mont_copy src3 (plusPtr tgt 96)
      c_bls12_381_Fp_mont_copy src4 (plusPtr tgt 144)
      c_bls12_381_Fp_mont_copy src5 (plusPtr tgt 192)
      c_bls12_381_Fp_mont_copy src6 (plusPtr tgt 240)
  return (MkFp6 ftgt)
packListPrime _ = error "expecting a list of 6 Fp elements"

----------------------------------------

{-# NOINLINE exportToCDef #-}
exportToCDef :: String -> Fp6 -> String
exportToCDef name val = unsafePerformIO $ do
  ws <- peekFlat val
  return $ exportWordsToC name ws

{-# NOINLINE exportListToCDef #-}
exportListToCDef :: String -> [Fp6] -> String
exportListToCDef name vals = unsafePerformIO $ do
  ws <- mapM peekFlat vals
  return $ exportWordsArrayToC 36 name (concat ws)

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp6_mont_pow_gen" c_bls12_381_Fp6_mont_pow_gen :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> CInt -> IO ()

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
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withInteger expo $ \(nwords,ptr2) -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_pow_gen ptr1 ptr2 ptr3 (fromIntegral nwords)
  return (MkFp6 fptr3)

{-# NOINLINE pow #-}
powBigInt384 :: Fp6 -> BigInt384 -> Fp6
powBigInt384 (MkFp6 fptr1) (MkBigInt384 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_pow_gen ptr1 ptr2 ptr3 36
  return (MkFp6 fptr3)

----------------------------------------

foreign import ccall unsafe "bls12_381_Fp6_mont_batch_inv" c_bls12_381_Fp6_mont_batch_inv :: CInt -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE batchInv #-}
batchInv :: FlatArray Fp6 -> FlatArray Fp6
batchInv (MkFlatArray n fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray (n*36)
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_batch_inv (fromIntegral n) ptr1 ptr2
  return (MkFlatArray n fptr2)

----------------------------------------


foreign import ccall unsafe "bls12_381_Fp6_mont_is_zero" c_bls12_381_Fp6_mont_is_zero :: Ptr Word64 -> IO Word8

{-# NOINLINE isZero #-}
isZero :: Fp6 -> Bool
isZero (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp6_mont_is_zero ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp6_mont_is_one" c_bls12_381_Fp6_mont_is_one :: Ptr Word64 -> IO Word8

{-# NOINLINE isOne #-}
isOne :: Fp6 -> Bool
isOne (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp6_mont_is_one ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp6_mont_is_equal" c_bls12_381_Fp6_mont_is_equal :: Ptr Word64 -> Ptr Word64 -> IO Word8

{-# NOINLINE isEqual #-}
isEqual :: Fp6 -> Fp6 -> Bool
isEqual (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_is_equal ptr1 ptr2
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp6_mont_is_valid" c_bls12_381_Fp6_mont_is_valid :: Ptr Word64 -> IO Word8

{-# NOINLINE isValid #-}
isValid :: Fp6 -> Bool
isValid (MkFp6 fptr) = unsafePerformIO $ do
  cret <- withForeignPtr fptr $ \ptr -> do
    c_bls12_381_Fp6_mont_is_valid ptr
  return (cret /= 0)

foreign import ccall unsafe "bls12_381_Fp6_mont_neg" c_bls12_381_Fp6_mont_neg :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE neg #-}
neg :: Fp6 -> Fp6
neg (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_neg ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bls12_381_Fp6_mont_add" c_bls12_381_Fp6_mont_add :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE add #-}
add :: Fp6 -> Fp6 -> Fp6
add (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_add ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bls12_381_Fp6_mont_sub" c_bls12_381_Fp6_mont_sub :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sub #-}
sub :: Fp6 -> Fp6 -> Fp6
sub (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_sub ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bls12_381_Fp6_mont_sqr" c_bls12_381_Fp6_mont_sqr :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE sqr #-}
sqr :: Fp6 -> Fp6
sqr (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_sqr ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bls12_381_Fp6_mont_mul" c_bls12_381_Fp6_mont_mul :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE mul #-}
mul :: Fp6 -> Fp6 -> Fp6
mul (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_mul ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bls12_381_Fp6_mont_inv" c_bls12_381_Fp6_mont_inv :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE inv #-}
inv :: Fp6 -> Fp6
inv (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_inv ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bls12_381_Fp6_mont_div" c_bls12_381_Fp6_mont_div :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE div #-}
div :: Fp6 -> Fp6 -> Fp6
div (MkFp6 fptr1) (MkFp6 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_bls12_381_Fp6_mont_div ptr1 ptr2 ptr3
  return (MkFp6 fptr3)

foreign import ccall unsafe "bls12_381_Fp6_mont_div_by_2" c_bls12_381_Fp6_mont_div_by_2 :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE divBy2 #-}
divBy2 :: Fp6 -> Fp6
divBy2 (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_div_by_2 ptr1 ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bls12_381_Fp6_mont_pow_uint64" c_bls12_381_Fp6_mont_pow_uint64 :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pow_ #-}
pow_ :: Fp6 -> Word64 -> Fp6
pow_ (MkFp6 fptr1) x = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  cret <- withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_pow_uint64 ptr1 x ptr2
  return (MkFp6 fptr2)

foreign import ccall unsafe "bls12_381_Fp6_mont_frobenius" c_bls12_381_Fp6_mont_frobenius :: Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE frob #-}
frob :: Fp6 -> Fp6
frob (MkFp6 fptr1) = unsafePerformIO $ do
  fptr2 <- mallocForeignPtrArray 36
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      c_bls12_381_Fp6_mont_frobenius ptr1 ptr2
  return (MkFp6 fptr2)
