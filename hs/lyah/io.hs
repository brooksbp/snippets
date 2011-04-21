--main = putStrLn "hello, world"

-- *Main> :t putStrLn
-- putStrLn :: String -> IO ()

-- putStrLn takes a String and returns an I/O action that
-- has a result type of () empty tuple / unit.  I/O action
-- is something that will perform an action with a side-effect
-- and contain some return value inside it.
-- unit value = (), type = ()

-- *Main> :t putStrLn "hello, world"
-- putStrLn "hello, world" :: IO ()

-- An I/O action will be performed when we give it a name of
-- 'main' and then run the program.  Use 'do' syntax to glue
-- together serveral I/O actions into one...

-- main = do
--   putStrLn "Hello, what is your name?"
--   name <- getLine
--   putStrLn ("Hey " ++ name ++ ", nice to meet you")

-- main always has a type signature of
-- main :: IO something where something is a concrete type

-- *Main> :t getLine
-- getLine :: IO String

-- in prev example, could've written instead
-- foo <- putStrLn "Hello, what.."
-- and foo would have value ()

-- inside a 'do' block, the last action cannot be bound to
-- a name.

-- can also write _ <- putStrLn "BLAH" but that's useless so leave
-- out the binding...

-- cannot 'set' a var to I/O result.. must 'bind'. so.. can't do
-- name = getLine... must 'bind' and get the result of an I/O
-- action from '<-' operator

-- main = do
--   line <- getLine
--   if null line
--     then return ()
--     else (do
--              putStrLn $ reverseWords line
--              main)

-- reverseWords :: String->String
-- reverseWords = unwords . map reverse . words

-- in haskell, 'return' makes an I/O action out of a pure value
-- return "haha" has type IO String

-- main = do
--   a <- return "hell"
--   b <- return " yeah!"
--   putStrLn $ a ++ b

-- import Control.Monad

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

-- main = do
--   a <- getLine
--   b <- getLine
--   print [a,b]

