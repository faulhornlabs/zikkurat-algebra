-- | Wrappers around platform-specific code
-- (for testing purposes only!)

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module ZK.Algebra.BigInt.Platform where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Control.Monad

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import System.IO.Unsafe

--------------------------------------------------------------------------------  

-- uint8_t wrap_addcarry_u64 ( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt );
-- uint8_t wrap_subborrow_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt );
-- uint8_t wrap_addcarry_u128_inplace( uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi);

foreign import ccall unsafe "wrap_addcarry_u64"  c_addcarry_u64  :: Word8 -> Word64 -> Word64 -> Ptr Word64 -> IO Word8
foreign import ccall unsafe "wrap_subborrow_u64" c_subborrow_u64 :: Word8 -> Word64 -> Word64 -> Ptr Word64 -> IO Word8
foreign import ccall unsafe "wrap_addcarry_u128_inplace" c_addcarry_u128_inplace :: Ptr Word64 -> Ptr Word64 -> Word64 -> Word64 -> IO Word8

{-# NOINLINE addCarry64 #-}
addCarry64 :: Bool -> Word64 -> Word64 -> (Bool, Word64)
addCarry64 carry arg1 arg2 = unsafePerformIO $ do
  alloca $ \ptr -> do
    d   <- c_addcarry_u64 (if carry then 1 else 0) arg1 arg2 ptr
    out <- peek ptr
    return (d /= 0, out)

{-# NOINLINE subBorrow64 #-}
subBorrow64 :: Bool -> Word64 -> Word64 -> (Bool, Word64)
subBorrow64 carry arg1 arg2 = unsafePerformIO $ do
  alloca $ \ptr -> do
    d   <- c_subborrow_u64 (if carry then 1 else 0) arg1 arg2 ptr
    out <- peek ptr
    return (d /= 0, out)

{-# NOINLINE addCarry128 #-}
addCarry128 :: (Word64,Word64) -> (Word64,Word64) -> (Bool, (Word64,Word64))
addCarry128 (lo1,hi1) (lo2,hi2) = unsafePerformIO $ do
  alloca $ \ptrLo -> alloca $ \ptrHi -> do
    poke ptrLo lo1
    poke ptrHi hi1
    d     <- c_addcarry_u128_inplace ptrLo ptrHi lo2 hi2
    outLo <- peek ptrLo
    outHi <- peek ptrHi
    return (d /= 0, (outLo,outHi))

