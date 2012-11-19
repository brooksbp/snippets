import Network.Socket
import Network.BSD
import Control.Concurrent
import System.IO
import Data.Bits
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serve :: String -> HandlerFunc -> IO ()
serve portStr handler =
  do addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing
                  (Just portStr)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock ReuseAddr 1
     bindSocket sock (addrAddress serveraddr)
     listen sock 5
     procRequests sock
  where
    procRequests :: Socket -> IO ()
    procRequests mastersock =
      do (connsock, clientaddr) <- accept mastersock
         handler clientaddr "client connected"
         forkIO $ procMessages connsock clientaddr
         procRequests mastersock
    procMessages :: Socket -> SockAddr -> IO ()
    procMessages connsock clientaddr =
      do connhdl <- socketToHandle connsock ReadMode
         hSetBuffering connhdl LineBuffering
         messages <- hGetContents connhdl
         mapM_ (handler clientaddr) (lines messages)
         hClose connhdl
         handler clientaddr "client disconnected"

plainHandler :: HandlerFunc
plainHandler clientaddr msg =
  putStrLn $ "From" ++ show clientaddr ++ ": " ++ msg
