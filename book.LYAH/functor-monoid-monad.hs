import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

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

-- instance Functor ((->) r) where
--   fmap = (.)

-- *Main> let a = fmap (*) [1,2,3,4]
-- *Main> :t a
-- a :: [Integer -> Integer]
-- *Main> fmap (\f -> f 9) a
-- [9,18,27,36]

-- class (Functor f) => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- pure should take a value of any type and return an applicative functor
-- with that value inside it. better way of thinking about pure is that
-- it takes a value and puts it in some default (or pure) context -- a
-- minimal context that still yeilds that value.
-- fmap takes a function and a functor and applies the function inside
-- the functor. <*> takes a functor that has a function in it and another
-- functor and extracts that function from the first functor and then
-- maps it over the second one.

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> something = fmap f something

-- *Main> Just (+3) <*> Just 9
-- Just 12
-- *Main> pure (+3) <*> Just 10
-- Just 13
-- *Main> pure (+) <*> Just 2 <*> Just 5
-- Just 7

-- pure f <*> x equals fmap f x
-- instead of writing: pure f <*> x <*> y <*> ..., write: fmap f x <*> y <*> ...
-- Control.Applicative exports a function <$> which is fmap as an infix op

-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- *Main> (++) <$> Just "john " <*> Just "travolta"
-- Just "john travolta"

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = [f x | f <- fs, x <- xs]

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

-- instance Applicative IO where
--   pure = return
--   a <*> b = do
--     f <- a
--     x <- b
--     return (f x)

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

-- instance Applicative ((->) r) where
--   pure x = (\_ -> x)
--   f <*> g = \x -> f x (g x)

-- *Main> (pure 3) "Blah"
-- 3
-- *Main> :t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: Num b => b -> b
-- *Main> (+) <$> (+3) <*> (*100) $ 5
-- 508
-- *Main> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- [8.0,10.0,2.5]

-- instance Applicative ZipList where
--   pure x = ZipList (repeat x)
--   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- <*> applies the first function to the first value, the second function
-- to the second value, etc..

-- pure "haha" equals ZipList (["haha","haha",...

-- *Main> :t getZipList
-- getZipList :: ZipList a -> [a]
-- *Main> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101,102,103]
-- *Main> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "bird"
-- [('d','c','b'),('o','a','i'),('g','t','r')]
-- (,,) equals \x y z -> (x,y,z)

-- *Main> :t liftA2
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b

-- *Main> fmap (\x -> [x]) (Just 4)
-- Just [4]

-- *Main> liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
-- *Main> (:) <$> Just 3 <*> Just [4]
-- Just [3,4]

sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA [] = pure []
-- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA = foldr (liftA2 (:)) (pure [])

-- *Main> sequenceA [Just 3, Just 2, Just 1]
-- Just [3,2,1]
-- *Main> sequenceA [Just 3, Nothing, Just 1]
-- Nothing
-- *Main> sequenceA [(+3),(+2),(+1)] 3
-- [6,5,4]
-- *Main> sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- *Main> sequenceA [[1,2,3],[4,5,6],[]]
-- []

-- does number satisfy all predicates in list?

-- *Main> map (\f -> f 7) [(>4),(<10),odd]
-- [True,True,True]
-- *Main> and $ map (\f -> f 7) [(>4),(<10),odd]
-- True
-- *Main> and $ sequenceA [(>4),(<10),odd] 7
-- True

{- Ways of list type being an applicative functor:
   <*> - take functions out of list to left, apply to
         every element of list to right. every possible
         combination of applying functions to elemnts
   ZipList - take 1st function from list to left apply to
             1st arg in list to right, 2nd ..., all the way down.
   
*Main> [(+1),(*100),(*5)] <*> [1,2,3]
[2,3,4,100,200,300,5,10,15]
*Main> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
[2,200,15]

data ZipList a = ZipList [a]
data ZipList a = ZipList { getZipLists :: [a] }

use newtype when you want to take a type and wrap it in something else.
e.g. ZipList a == [a]

newtype ZipList a = ZipList { getZipList :: [a] }

newtype is faster. 'data' boxes things...

when you use newtype, can only have one value constructor with one field.
with data you can have several value constructors with zero or more fields. -}

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq,Show)
-- *Main> :t CharList
-- CharList :: [Char] -> CharList

{-

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
   fmap ...

implement fmap, looks something like
fmap :: (a -> b) -> Mayber a -> Maybe b

what if we want to make tuple an instance of Functor such that fmap
only applys f to the first component of tuple? e.g.
fmap (+3) (1,1) equals (4,1)
writing that instance is kind of hard, so use newtype Pair :) -}

newtype Pair b a = Pair { getPair :: (a,b) }
instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)
--fmap :: (a -> b) -> Pair c a -> Pair c b
-- *Main> getPair $ fmap (*100) (Pair (2,3))
-- (200,3)


-- *Main> undefined
-- *** Exception: Prelude.undefined
-- *Main> head [3,4,undefined,6]
-- 3

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- *Main> helloMe undefined
-- "*** Exception: Prelude.undefined

-- data can have multiple value constructors, so to see if our function conforms to
-- (CoolBool _) pattern, Haskell has to evaluate the value..

newtype BetterBool = BetterBool { getBetterBool :: Bool }

helloMe' :: BetterBool -> String
helloMe' (BetterBool _) = "hello"

-- *Main> helloMe' undefined
-- "hello"

-- LAZIER...


{- type keyword for making type synonyms
   newtype keyword for taking existing types and wrapping in new types
     easier to make instance of certain type classes
     record syntax for newtype gets you functions for converting between the new type and original type
   data keyword for making unrestricted data types.

   1) if you want type signatures to look cleaner and be more descriptive, use type synonyms.
   2) if you want to take an existing type and wrap it in a new type in order to make it
      an instance of a type class, use newtype.
   3) if you want to make something completely new, use data. -}

{- Monoids

   type classes give an interface for types that have some behavior in common.
   e.g. Eq for types whose values can be equated, Functor, Applicative.

   when we make a type we think about what behavior it supports aka what it can act like
   think if there's already an existing type class

   *, ++ [], [] ++
   take 2 parameters, parameters & return have same type, there exists a value that doesn't change other values when evaluated.
   when we have 3 or more values, use binary function to reduce to single result e.g. (3 * $) * 5  think about associativity

   A monoid is when you have an associative binary function and a value which acts as an identity wrt that function. -}

--import Data.Monoid
-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

{- only concrete types can be made isntances of Monoid

   mempty isn't really a function since it doesnt take parameters, it's a polymorphic constant
   it's the identity of that monoid

   mappend, the binary function
   mconcat, takes a list of monoid values and reduces them to single value via mappend. default implementation takes mempty as starting value and folds list from right with mappend. can define if want to implement more efficiently

   monoid rules:
   mempty `mappend` x = x
   x `mappend` mempty = x
   (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}

-- instance Monoid [a] where
--   mempty = []
--   mappend = (++)

-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   Nothing `mappend` m = m
--   m `mappend` Nothing = m
--   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- *Main> Nothing `mappend` Just "andy"
-- Just "andy"
-- *Main> Just LT `mappend` Nothing
-- Just LT
-- *Main> Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})

-- newtype First a = First { getFirst :: Maybe a }
--                 deriving (Eq, Ord, Read, Show)
-- -- wrap Maybe a in a First.

-- instance Monoid (First a) where
--   mempty = First Nothing
--   First (Just x) `mappend` _ = First (Just x)
--   First Nothing `mappend` x = x

-- *Main> getFirst $ First (Just 'a') `mappend` First (Just 'b')
-- Just 'a'
-- *Main> getFirst $ First Nothing `mappend` First (Just 'b')
-- Just 'b'
-- *Main> getFirst $ First (Just 'a') `mappend` First Nothing
-- Just 'a'
-- *Main> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- Just 9
-- *Main> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
-- Just 10
-- *Main> getLast $ Last (Just "one") `mappend` Last (Just "two")
-- Just "two"

-- Functor is for things that can be mapped over, Foldable is for things that can be folded up

-- *Main> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- *Main> :t F.foldr
-- F.foldr :: F.Foldable t => (a -> b -> b) -> b -> t a -> b
-- *Main> foldr (*) 1 [1,2,3]
-- 6
-- *Main> F.foldr (*) 1 [1,2,3]
-- 6
-- *Main> F.foldl (+) 2 (Just 9)
-- 11
-- *Main> F.foldr (||) False (Just True)
-- True

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Read, Eq)

-- *Main> :t F.foldMap
-- F.foldMap :: (Monoid m, F.Foldable t) => (a -> m) -> t a -> m

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x `mappend`
                           F.foldMap f r

-- F.foldl (+) 0 testTree
-- F.foldl (*) 1 testTree

-- is any number in tree equal to 3?
-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- True
-- convert Tree into List
-- F.foldMap (\x -> [x]) testTree
-- [1,3,6,5,8,9,10]

{- Monads

   Functors are useful concept for values that can be mapped over. Applicative
   functors allows viewing values of certain data types as values with contexts
   and use normal functions on those values while preserving contexts..?

   monads are beefed up applicative functors

   fmap :: (Functor f) => (a -> b) -> f a -> f b

   what if function is already wrapped inside a functor value? like if we had
   Just (*3) and wanted to apply to Just 5 ?

   (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

   monads are concerned with: if you have a value with a context 'm a' how do you apply
   a function to it that takes a normal 'a' and returns a value with a context? e.g. how
   do you apply a function of type 'a -> m b' to a value of type 'm a'

   (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

   -}