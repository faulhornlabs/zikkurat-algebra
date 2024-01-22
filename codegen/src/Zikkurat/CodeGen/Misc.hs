
module Zikkurat.CodeGen.Misc where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits
import Data.Char

import Text.Printf

import System.FilePath
import System.Directory

--------------------------------------------------------------------------------

data HsOrC 
  = Hs 
  | C
  deriving (Eq,Show)

parseHsOrC :: String -> Maybe HsOrC
parseHsOrC str = case map toLower str of
  "c"       -> Just C
  "hs"      -> Just Hs
  "haskell" -> Just Hs
  _         -> Nothing

--------------------------------------------------------------------------------

partitionIntoChunks :: Int -> [a] -> [[a]]
partitionIntoChunks k = go where
  go [] = []
  go xs = take k xs : go (drop k xs)

--------------------------------------------------------------------------------

-- | A path (module or c source file or c header). The last entry is the 
-- the base filename (no extension!)
newtype Path 
  = Path [String]
  deriving (Eq,Show)

pathDirectory :: Path -> FilePath
pathDirectory (Path path) = foldl (</>) "." (init path)

pathBaseName :: Path -> String
pathBaseName (Path path) = last path

pathReplaceBaseName :: String -> Path -> Path
pathReplaceBaseName new (Path path) = Path (init path ++ [new])

cFilePath :: String -> Path -> FilePath
cFilePath ext (Path path) = intercalate "/" path ++ "." ++ ext

hsFilePath :: Path -> FilePath
hsFilePath (Path path) = intercalate "/" path ++ ".hs"

hsModule :: Path -> FilePath
hsModule (Path path) = intercalate "." path

--------------------------------------------------------------------------------

-- | Given a file, we create it's directory if necessary
createTgtDirectory :: FilePath -> IO ()
createTgtDirectory fpath = createDirectoryIfMissing True (takeDirectory fpath)

--------------------------------------------------------------------------------

showHex64 :: Word64 -> String
showHex64 = printf "0x%016x"

wordsToCHexString :: String -> [Word64] -> String
wordsToCHexString cname input = unlines ([header]++stuff++[finish]) where
  header = "uint64_t " ++ cname ++ "[" ++ show (length input) ++ "] ="
  stuff  = zipWith f ('{':repeat ',') $ partitionIntoChunks 4 input
  finish = "  };"
  f ch ws = "  " ++ [ch] ++ " " ++ intercalate ", " (map showHex64 ws)

--------------------------------------------------------------------------------

fromWord64sLE :: [Word64] -> Integer
fromWord64sLE = go where
  go []     = 0
  go (x:xs) = fromIntegral x + shiftL (go xs) 64

toWord64sLE :: Integer -> [Word64]
toWord64sLE = go where
  go 0 = []
  go k = fromInteger (k .&. (2^64-1)) : go (shiftR k 64)

toWord64sLE' :: Int -> Integer -> [Word64]
toWord64sLE' len what = take len $ toWord64sLE what ++ repeat 0

--------------------------------------------------------------------------------

mkConst :: Int -> String -> Integer -> String 
mkConst n name value = "const uint64_t " ++ name ++ "[" ++ show n ++ "] = { " ++ intercalate ", " (map showHex64 ws) ++ " };" where
  ws = toWord64sLE' n value

mkConstArr :: Int -> String -> [Integer] -> String 
mkConstArr n name values = "const uint64_t " ++ name ++ "[" ++ show (n*length values) ++ "] = { " ++ intercalate ", " (map showHex64 $ concat wss) ++ " };" where
  wss = map (toWord64sLE' n) values

-- | Note: here @n@ must be the number of limbs in Fp2, not in Fp!
mkConstFp2 :: Int -> String -> (Integer,Integer) -> String 
mkConstFp2 n name (value1,value2) = "const uint64_t " ++ name ++ "[" ++ show n ++ "] = { " ++ intercalate ", " (map showHex64 ws) ++ " };" where
  ws  = (ws1 ++ ws2)
  ws1 = toWord64sLE' (div n 2) value1
  ws2 = toWord64sLE' (div n 2) value2

-- | Note: here @n@ must be the number of limbs in Fp2, not in Fp!
mkConstArrFp2 :: Int -> String -> [(Integer,Integer)] -> String 
mkConstArrFp2 n name values = "const uint64_t " ++ name ++ "[" ++ show (n*length values) ++ "] = { " ++ intercalate ", " (map showHex64 $ concat wss) ++ " };" where
  wss = map f values
  f (value1,value2) = ws1 ++ ws2 where
    ws1 = toWord64sLE' (div n 2) value1
    ws2 = toWord64sLE' (div n 2) value2

--------------------------------------------------------------------------------

type CName  = String
type HsName = String

--------------------------------------------------------------------------------

type Code = [String]

catCode :: [Code] -> Code
catCode = concat . intersperse [""]

pprint :: Code -> IO ()
pprint ls = mapM_ putStrLn ("":ls)

--------------------------------------------------------------------------------

index :: Int -> String -> String
index j var = var ++ "[" ++ show j ++ "]"

advance :: Int -> String -> String
advance j var = var ++ "+" ++ show j

--------------------------------------------------------------------------------
