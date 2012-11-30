--module Main (main) where

import GHC.Word
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put

data Frame =
  Frame { header :: Header
        , msg    :: Msg
        } deriving (Show)

instance Serialize Frame where
  put (Frame (Header _ _) msg) = do
    putWord8 $ msgType msg
    let bs = runPut $ putMsg msg
    put ((fromIntegral $ BS.length bs) :: Word16)
    putByteString bs
  get = do
    hdr <- get :: Get Header
    msg <- getMsg hdr
    return $ Frame hdr msg

data Header =
  Header { ty      :: Word8
         , len     :: Word16
         } deriving (Show)

instance Serialize Header where
  put (Header ty len) = do
    putWord8 ty
    putWord16be len
  get = do
    ty  <- liftM fromIntegral getWord8
    len <- liftM fromIntegral getWord16be
    return (Header ty len)

data Msg = MsgPing
         | MsgError Error
         deriving (Show)

msgType (MsgPing)    = 0
msgType (MsgError _) = 1

data Error = PermError PermErrorCode String
           | ArgError ArgErrorCode
           deriving (Show)

data PermErrorCode = PermErrorResource
                   | PermErrorInUse
                   deriving (Show, Enum)

data ArgErrorCode = ArgErrorLen
                  | ArgErrorType
                  deriving (Show, Enum)

putMsg :: Msg -> Put
putMsg (MsgPing) = putByteString BS.empty
putMsg (MsgError (PermError code string)) = do
  putWord16be 0
  putWord16be $ fromIntegral $ fromEnum code
  putByteString $ C.pack string
putMsg (MsgError (ArgError code)) = do
  putWord16be 1
  putWord16be $ fromIntegral $ fromEnum code

getMsg (Header ty len) =
  case ty of
    0 -> return MsgPing
    1 -> getMsgError (fromIntegral len)

getMsgError :: Int -> Get Msg
getMsgError len = do
  errTy <- liftM fromIntegral getWord16be
  code  <- liftM fromIntegral getWord16be
  MsgError <$> case errTy of
    0 -> do
      bs <- getByteString (len - 4)
      return $ PermError (toEnum code :: PermErrorCode) (C.unpack bs)
    1 -> return $ ArgError (toEnum code :: ArgErrorCode)

main :: IO ()
main = do
  let bs = encode (Frame (Header 0 0) (MsgError (ArgError ArgErrorLen)))
  let str = case runGet get bs of
        Left err -> err
        Right fr -> show (fr :: Frame)
  putStrLn $ "frame: " ++ str
  let bs = encode (Frame (Header 0 0) (MsgError (PermError PermErrorResource "msg")))
  putStrLn $ case runGet get bs of
    Left err -> err
    Right fr -> show (fr :: Frame)
