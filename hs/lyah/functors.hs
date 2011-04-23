
{-functors are things that can be mapped over. described by Functor typeclass
  which only has one typeclass method fmap :: (a -> b) -> f a -> f b which
  reads "give me a function that takes a 'a' and returns a 'b' and a box full
  of 1+ 'a's and I'll return a box with 1+ 'b's.

  If we make a type constructor an instance of Functor it must have kind
  * -> * which means it must take exactly one concrete type as a type param.
  So Maybe can be made instances b/c Maybe Int, Maybe String
  But not Either, must partially apply Either like (Either a) where
  fmap :: (b -> c) -> Either a b -> Either a c -}

-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- main = do line <- getLine
--           let line' = reverse line
--           putStrLn $ "backwards: " ++ line'
-- can be written as
-- main = do line <- fmap reverse getLine
--           putStrLn $ "backwards: " ++ line

{- use fmap if you bind the result of an I/O action to a name just to apply
   a function to it. can use composition too, -}

-- import Data.Char
-- import Data.List
-- main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
--           putStrLn line

instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

{- fmap :: (a -> b) -> f a -> f b
   fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
   fmap :: (a -> b) -> (r -> a) -> (r -> b)

   takes a function from 'a' to 'b' and a function from 'r' to 'a' and returns
   a function from 'r' to 'b'. this is like function composition.-}