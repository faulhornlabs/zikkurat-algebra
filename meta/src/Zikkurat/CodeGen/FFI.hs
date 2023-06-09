
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

-- TODO: put these in their own modules
hsMiscTmp :: Code
hsMiscTmp =
  [ "fromWord64sLE :: [Word64] -> Integer"
  , "fromWord64sLE = go where"
  , "  go []     = 0"
  , "  go (x:xs) = fromIntegral x + shiftL (go xs) 64"
  , ""
  , "toWord64sLE :: Integer -> [Word64]"
  , "toWord64sLE = go where"
  , "  go 0 = []"
  , "  go k = fromInteger (k .&. (2^64-1)) : go (shiftR k 64)"
  , ""
  , "toWord64sLE' :: Int -> Integer -> [Word64]"
  , "toWord64sLE' len what = take len $ toWord64sLE what ++ repeat 0"
  ]

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

  (CTyp [CArgInPtr,CArg64,CArgOutPtr] CRetVoid) ->
    [ "foreign import ccall unsafe \"" ++ cname ++ "\" c_" ++ cname ++ " :: Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()"
    , ""
    , "{-# NOINLINE " ++ hsFunName ++ " #-}"
    , hsFunName ++ " :: " ++ hsTyName ++ " -> Word64 -> " ++ hsTyName
    , hsFunName ++ " (" ++ hsTyCon ++ " fptr1) x = unsafePerformIO $ do"
    , "  fptr2 <- mallocForeignPtrArray " ++ show hsNLimbs
    , "  cret <- withForeignPtr fptr1 $ \\ptr1 -> do"
    , "    withForeignPtr fptr2 $ \\ptr2 -> do"
    , "      c_" ++ cname ++ " ptr1 x ptr2"
    , "  return (" ++ hsTyCon ++ " fptr2)"
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

  _ -> error $ "CodeGen.FFI.ffiCall: C function type not implemented:\n    " ++ show cfunty

--------------------------------------------------------------------------------

ffiMarshal :: String -> String -> Int -> Code
ffiMarshal postfix typeName nlimbs =
  [ "{-# NOINLINE unsafeMk" ++ postfix ++ " #-}"
  , "unsafeMk" ++ postfix ++ " :: Integer -> IO " ++ typeName
  , "unsafeMk" ++ postfix ++ " x = do"
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
  , "{-# NOINLINE unsafeTo" ++ postfix ++ " #-}"
  , "unsafeTo" ++ postfix ++ " :: Integer -> " ++ typeName ++ ""
  , "unsafeTo" ++ postfix ++ " x = unsafePerformIO (unsafeMk" ++ postfix ++ " x)"
  , ""
  , "{-# NOINLINE from" ++ postfix ++ " #-}"
  , "from" ++ postfix ++ " :: " ++ typeName ++ " -> Integer"
  , "from" ++ postfix ++ " f = unsafePerformIO (get" ++ postfix ++ " f)"
  ]

--------------------------------------------------------------------------------
