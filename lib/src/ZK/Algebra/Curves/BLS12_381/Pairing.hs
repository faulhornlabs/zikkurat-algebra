-- | Optimal Ate pairing for BLS12_381 curve

-- NOTE 1: This module is intented to be imported qualified
-- NOTE 2: Generated code, do not edit!

{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies #-}
module ZK.Algebra.Curves.BLS12_381.Pairing
  ( pairing
  , pairingProj
  )
  where

--------------------------------------------------------------------------------
  
import Control.Monad
import System.IO.Unsafe

import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import ZK.Algebra.Class.Field   as F
import ZK.Algebra.Class.Curve   as C
import qualified ZK.Algebra.Class.Pairing as P

import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr   )
import ZK.Algebra.Curves.BLS12_381.Poly    ( Poly )

import ZK.Algebra.Curves.BLS12_381.Fp.Mont   ( Fp   )
import ZK.Algebra.Curves.BLS12_381.Fp2.Mont  ( Fp2  )
import ZK.Algebra.Curves.BLS12_381.Fp6.Mont  ( Fp6  )
import ZK.Algebra.Curves.BLS12_381.Fp12.Mont ( Fp12 )

import ZK.Algebra.Curves.BLS12_381.G1.Affine ( G1 )
import ZK.Algebra.Curves.BLS12_381.G2.Affine ( G2 )

import qualified ZK.Algebra.Curves.BLS12_381.Fr.Mont   as Fr
import qualified ZK.Algebra.Curves.BLS12_381.Fp.Mont   as Fp
import qualified ZK.Algebra.Curves.BLS12_381.Fp2.Mont  as Fp2
import qualified ZK.Algebra.Curves.BLS12_381.Fp6.Mont  as Fp6
import qualified ZK.Algebra.Curves.BLS12_381.Fp12.Mont as Fp12

import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine as AffG1
import qualified ZK.Algebra.Curves.BLS12_381.G2.Affine as AffG2
import qualified ZK.Algebra.Curves.BLS12_381.G1.Proj   as ProjG1
import qualified ZK.Algebra.Curves.BLS12_381.G2.Proj   as ProjG2

--------------------------------------------------------------------------------

type ProjG1 = ProjG1.G1
type ProjG2 = ProjG2.G2

--------------------------------------------------------------------------------

instance P.PairingCurve 'P.BLS12_381 where
  type Fp     'P.BLS12_381 = Fp.Fp
  type Fp2    'P.BLS12_381 = Fp2.Fp2
  type Fp12   'P.BLS12_381 = Fp12.Fp12
  type Fr     'P.BLS12_381 = Fr.Fr
  type G1     'P.BLS12_381 = AffG1.G1
  type G2     'P.BLS12_381 = AffG2.G2
  type ProjG1 'P.BLS12_381 = ProjG1.G1
  type ProjG2 'P.BLS12_381 = ProjG2.G2
  type Poly   'P.BLS12_381 = Poly

  pairing _proxy = ZK.Algebra.Curves.BLS12_381.Pairing.pairing

--------------------------------------------------------------------------------

-- void bls12_381_pairing_affine    (const uint64_t *P, const uint64_t *Q, uint64_t *tgt);
-- void bls12_381_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt);

foreign import ccall unsafe "bls12_381_pairing_affine"     c_pairing_affine     :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()
foreign import ccall unsafe "bls12_381_pairing_projective" c_pairing_projective :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

{-# NOINLINE pairing #-}
pairing :: G1 -> G2 -> Fp12
pairing (AffG1.MkG1 fptr1) (AffG2.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_pairing_affine ptr1 ptr2 ptr3
  return (Fp12.MkFp12 fptr3)

{-# NOINLINE pairingProj #-}
pairingProj :: ProjG1 -> ProjG2 -> Fp12
pairingProj (ProjG1.MkG1 fptr1) (ProjG2.MkG2 fptr2) = unsafePerformIO $ do
  fptr3 <- mallocForeignPtrArray 72
  withForeignPtr fptr1 $ \ptr1 -> do
    withForeignPtr fptr2 $ \ptr2 -> do
      withForeignPtr fptr3 $ \ptr3 -> do
        c_pairing_projective ptr1 ptr2 ptr3
  return (Fp12.MkFp12 fptr3)

