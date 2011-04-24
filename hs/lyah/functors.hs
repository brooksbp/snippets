
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

-- instance Functor ((->) r) where
--   fmap f g = (\x -> f (g x))

{- fmap :: (a -> b) -> f a -> f b
   fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
   fmap :: (a -> b) -> (r -> a) -> (r -> b)

   takes a function from 'a' to 'b' and a function from 'r' to 'a' and returns
   a function from 'r' to 'b'. this is like function composition.-}

instance Functor ((->) r) where
  fmap = (.)

-- *Main> let a = fmap (*) [1,2,3,4]
-- *Main> :t a
-- a :: [Integer -> Integer]
-- *Main> fmap (\f -> f 9) a
-- [9,18,27,36]

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- pure should take a value of any type and return an applicative functor
-- with that value inside it. better way of thinking about pure is that
-- it takes a value and puts it in some default (or pure) context -- a
-- minimal context that still yeilds that value.
-- fmap takes a function and a functor and applies the function inside
-- the functor. <*> takes a functor that has a function in it and another
-- functor and extracts that function from the first functor and then
-- maps it over the second one.

instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something

-- *Main> Just (+3) <*> Just 9
-- Just 12
-- *Main> pure (+3) <*> Just 10
-- Just 13
-- *Main> pure (+) <*> Just 2 <*> Just 5
-- Just 7

-- pure f <*> x equals fmap f x
-- instead of writing: pure f <*> x <*> y <*> ..., write: fmap f x <*> y <*> ...
-- Control.Applicative exports a function <$> which is fmap as an infix op

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- *Main> (++) <$> Just "john " <*> Just "travolta"
-- Just "john travolta"

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

-- *Main> pure "Hey" :: [String]
-- ["Hey"]
-- *Main> pure "Hey" :: Maybe String
-- Just "Hey"

-- *Main> [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]

-- *Main> [(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]
-- [(+),(*)] <*> [1,2] ===> [(1+),(2+),(1*),(2*)] ===> ...

-- *Main> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- *Main> [ x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]
-- *Main> (*) <$> [2,5,10] <*> [8,10,11]
-- [16,20,22,40,50,55,80,100,110]
-- *Main> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
-- [55,80,100,110]

instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

myAction :: IO String
-- myAction = do
--   a <- getLine
--   b <- getLine
--   return $ a ++ b
myAction = (++) <$> getLine <*> getLine

-- main = do
--   a <- myAction
--   putStrLn $ "Two concated lines: " ++ a

-- if you ever bind I/O actions to names then call some function on them and wrap
-- the result in a return, consider applicative style like above

instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)

