-- file: ch27/SyslogClient.hs

{- For debian distributions, enable TCP/UDP reception:
   sudo emacs -nw /etc/rsyslog.conf
   sudo /etc/init.d/rsyslog restart
   
   tail -f /var/log/syslog

   ghci
   *Main> :load SyslogClient.hs
   *Main> h <- openlog "localhost" "514" "testprog"
   *Main> loading...
   *Main> syslog h USER WARNING "Will you pay attention to me now?"
   *MAIN> closelog h
-}

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import SyslogTypes

data SyslogHandle =
  SyslogHandle {slSocket :: Socket,
                slProgram :: String,
                slAddress :: SockAddr}
  
openlog :: HostName -> String -> String -> IO SyslogHandle
openlog hostname port progname =
  do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     return $ SyslogHandle sock progname (addrAddress serveraddr)

closelog :: SyslogHandle -> IO ()
closelog syslogh = sClose (slSocket syslogh)

makeCode :: Facility -> Priority -> Int
makeCode fac pri = 
  let faccode = codeOfFac fac
      pricode = fromEnum pri
      in (faccode `shiftL` 3) .|. pricode

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg = sendstr sendmsg
  where code = makeCode fac pri
        sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++ ": " ++ msg
        
        sendstr :: String -> IO ()
        sendstr [] = return ()
        sendstr omsg = do sent <- sendTo (slSocket syslogh) omsg (slAddress syslogh)
                          sendstr (genericDrop sent omsg)

