module Main (main) where

import GHC.Word
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

data Header =
  Header { ty      :: Word8
         , len     :: Word16
         } deriving (Show)

instance Serialize Header where
  put (Header ty len) = do
    putWord8 ty
    putWord16be len
  get = do
    ty  <- getWord8    >>= return . fromIntegral
    len <- getWord16be >>= return . fromIntegral
    return (Header ty len)

main :: IO ()
main = do
  let bs = encode (Header 1 2)
  let str = case (runGet get bs) of
        Left err -> err
        Right fr -> show (fr :: Header)
  putStrLn $ "hdr or err: " ++ str

