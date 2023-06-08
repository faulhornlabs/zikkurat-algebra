
-- | Generate FFI between Haskell and C code

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module Zikkurat.CodeGen.FFI where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits

import Control.Monad
import System.FilePath

import Zikkurat.CodeGen.Misc

--------------------------------------------------------------------------------

data CRet 
  = CRetVoid       -- ^ returns void
  | CRetBool       -- ^ returns bool or carry (as uint8)
  | CRet64         -- ^ retursn a 64 bit word
  deriving (Eq, Show)

data CArg
  = CArgInt        -- ^ C integer
  | CArg64         -- ^ 64 bit word
  | CArgInPtr      -- ^ input ptr
  | CArgOutPtr     -- ^ output ptr
  deriving (Eq, Show)

data CTyp 
  = CTyp [CArg] CRet
  deriving (Eq,Show)

data CFun 
  = CFun String CTyp
  deriving (Eq,Show)

--------------------------------------------------------------------------------

data HsTyDesc = HsTyDesc 
  { hsTyName :: HsName          -- ^ type name (eg. @BigInt256@)
  , hsTyCon  :: HsName          -- ^ constructor name (eg. @MkBigInt256@)
  , hsNLimbs :: Int             -- ^ number of words in the underlying foreignptr
  }
  deriving Show

--------------------------------------------------------------------------------

ffiCall :: HsTyDesc -> HsName -> CFun -> Code
ffiCall HsTyDesc{..} hsFunName cfunty@(CFun cname ctyp) = case ctyp of

  CTyp [CArgInPtr] CRetBool -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> IO Word8"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> Bool" 
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr) = unsafePerformIO $ do"
    , "  cret <- withForeignPtr fptr $ \\ptr -> do"
    , "    c_" ++ cname ++ " ptr"
    , "  return (cret /= 0)"
    ]

  CTyp [CArgInPtr,CArgInPtr] CRetBool -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Ptr Word64 -> IO Word8"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> " ++ hsTyName ++ " -> Bool" 
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) (" ++ hsTyCon ++ " fptr2) = unsafePerformIO $ do"
    , "  cret <- withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr1 $ \\ptr2 -> do"
    , "      c_" ++ cname ++ " ptr1 ptr2"
    , "  return (cret /= 0)"
    ]

  CTyp [CArgOutPtr, CArg64] CRetVoid -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Word64 -> IO ()"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: Word64 -> " ++ hsTyName 
    , hsFunName ++ " x = unsafePerformIO $ do"
    , "  fptr1 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    c_" ++ cname ++ " ptr1 x"
    , "  return (" ++ hsTyCon ++ " fptr1)"
    ]

  CTyp [CArgInPtr, CArgOutPtr] CRetVoid -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Ptr Word64 -> IO ()"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> " ++ hsTyName 
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) = unsafePerformIO $ do"
    , "  fptr2 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr2 $ \\ptr2 -> do"
    , "      c_" ++ cname ++ " ptr1 ptr2"
    , "  return (" ++ hsTyCon ++ " fptr2)"
    ]

  CTyp [CArgInPtr, CArgInPtr, CArgOutPtr] CRetVoid -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> " ++ hsTyName ++ " -> " ++ hsTyName
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) (" ++ hsTyCon ++ " fptr2) = unsafePerformIO $ do"
    , "  fptr3 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr2 $ \\ptr2 -> do"
    , "      withForeignPtr fptr3 $ \\ptr3 -> do"
    , "        c_" ++ cname ++ " ptr1 ptr2 ptr3"
    , "  return (" ++ hsTyCon ++ " fptr3)"
    ]

  CTyp [CArgInPtr, CArgOutPtr] CRetBool -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Ptr Word64 -> IO Word8"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> (" ++ hsTyName ++ ", Bool)"
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) = unsafePerformIO $ do"
    , "  fptr2 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  cret <- withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr2 $ \\ptr2 -> do"
    , "      c_" ++ cname ++ " ptr1 ptr2"
    , "  return (" ++ hsTyCon ++ " fptr2, cret /=0)"
    ]

  CTyp [CArgInPtr, CArgOutPtr, CArgInt] CRetVoid -> 
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Ptr Word64 -> CInt -> IO ()"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> Int -> " ++ hsTyName 
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) x = unsafePerformIO $ do"
    , "  fptr2 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr2 $ \\ptr2 -> do"
    , "      c_" ++ cname ++ " ptr1 ptr2 (fromIntegral x)"
    , "  return (" ++ hsTyCon ++ " fptr2)"
    ]

  _ -> error $ "CodeGen.FFI.ffiCall: C function type not implemented: " ++ show cfunty

--------------------------------------------------------------------------------

ffiMarshal :: String -> String -> Int -> Code
ffiMarshal postfix typeName nlimbs =
  [ "{-# NOINLINE mk" ++ postfix ++ " #-}"
  , "mk" ++ postfix ++ " :: Integer -> IO " ++ typeName
  , "mk" ++ postfix ++ " x = do"
  , "  fptr <- mallocForeignPtrArray " ++ show nlimbs
  , "  withForeignPtr fptr $ \\ptr -> do"
  , "    pokeArray ptr $ toWord64sLE' " ++ show nlimbs ++ " x"
  , "  return $ Mk" ++ typeName ++ " fptr"
  , ""
  , "{-# NOINLINE get" ++ postfix ++ " #-}"
  , "get" ++ postfix ++ " :: " ++ typeName ++ " -> IO Integer"
  , "get" ++ postfix ++ " (Mk" ++ typeName ++ " fptr) = do"
  , "  ws <- withForeignPtr fptr $ \\ptr -> peekArray " ++ show nlimbs ++ " ptr "
  , "  return (fromWord64sLE ws)"
  , ""
  , "{-# NOINLINE to" ++ postfix ++ " #-}"
  , "to" ++ postfix ++ " :: Integer -> " ++ typeName ++ ""
  , "to" ++ postfix ++ " x = unsafePerformIO (mk" ++ postfix ++ " x)"
  , ""
  , "{-# NOINLINE from" ++ postfix ++ " #-}"
  , "from" ++ postfix ++ " :: " ++ typeName ++ " -> Integer"
  , "from" ++ postfix ++ " f = unsafePerformIO (get" ++ postfix ++ " f)"
  ]

--------------------------------------------------------------------------------
