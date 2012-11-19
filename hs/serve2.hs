import Network.Socket
import Network.BSD
import Control.Concurrent
import Control.Monad
import System.IO
import Data.Bits
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

plainHandler :: HandlerFunc
plainHandler clientaddr msg =
  putStrLn $ "From " ++ show clientaddr ++ ": " ++ msg

serve :: String -> IO ()
serve portStr = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing
               (Just portStr)
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

bytesPerRead :: Int
bytesPerRead = 4096

serveConn :: Socket -> SockAddr -> IO ()
serveConn csocket caddr =
  forever $ do
    rbuf <- recv csocket bytesPerRead
    plainHandler caddr rbuf
