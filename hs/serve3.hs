import System.IO
import Control.Monad
import Control.Concurrent
import Network.BSD
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import qualified Data.ByteString as BS

type HandlerFunc = SockAddr -> BS.ByteString -> IO ()

plainHandler :: HandlerFunc
plainHandler clientaddr msg =
  putStrLn $ "From " ++ show clientaddr ++ ": " ++ (BS.unpack msg)

serve :: String -> IO ()
serve portStr = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just portStr)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  serveConns sock

serveConns :: Socket -> IO ()
serveConns socket =
  forever $ do
    (csocket, caddr) <- accept socket
    _ <- forkIO $ serveConn csocket caddr
    return ()

serveConn :: Socket -> SockAddr -> IO ()
serveConn csocket caddr =
  forever $ do
    msg <- recv csocket 4096
    plainHandler caddr msg
