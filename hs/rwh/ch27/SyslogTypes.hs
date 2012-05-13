-- file: ch27/SyslogTypes.hs

module SyslogTypes where

{- | Priorities define how important a log message is. -}

data Priority = DEBUG
              | INFO
              | NOTICE
              | WARNING
              | ERROR
              | CRITICAL
              | ALERT
              | EMERGENCY
                deriving (Eq, Ord, Show, Read, Enum)

{- | Facilities are used by the system to determine where
     messages are sent. -}

data Facility = KERN
              | USER
              | MAIL
              | DAEMON
              | AUTH
              | SYSLOG
              | LPR
              | NEWS 
              | UUCP
              | CRON
              | AUTHPRIV
              | FTP
              | LOCAL0
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

facToCode = [(KERN, 0),
             (USER, 1),
             (MAIL, 2),
             (DAEMON, 3),
             (AUTH, 4),
             (SYSLOG, 5),
             (LPR, 6),
             (NEWS, 7),
             (UUCP, 8),
             (CRON, 9),
             (AUTHPRIV, 10),
             (FTP, 11),
             (LOCAL0, 16),
             (LOCAL1, 17),
             (LOCAL2, 18),
             (LOCAL3, 19),
             (LOCAL4, 20),
             (LOCAL5, 21),
             (LOCAL6, 22),
             (LOCAL7, 23)
            ]

codeToFac = map (\(x, y) -> (y, x)) facToCode

{- | We can't use enum here because the numbering is discontiguous -}
codeOfFac :: Facility -> Int
codeOfFac f =
  case lookup f facToCode of
    Just x -> x
    _ -> error $ "Internal error codeOfFac"

facOfCode :: Int -> Facility
facOfCode f =
  case lookup f codeToFac of
    Just x -> x
    _ -> error $ "Invalid code in facOfCode"
