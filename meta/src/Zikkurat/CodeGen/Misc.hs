
module Zikkurat.CodeGen.Misc where

--------------------------------------------------------------------------------

import Data.List
import Data.Word
import Data.Bits
import Data.Char

import Text.Printf

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

showHex64 :: Word64 -> String
showHex64 = printf "0x%016x"

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
