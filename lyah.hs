import qualified Data.Map as Map


doubleMe x = x+x

doubleUs x y = doubleMe x + doubleMe y


--removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--factorial :: Integer -> Integer
--factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

fiveInt = read "5" :: Int

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first  :: (a, b, c) -> a
first  (x, _, _) = x
second :: (a, b, c) -> b
second (_, y, _) = y
third  :: (a, b, c) -> c
third  (_, _, z) = z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string, brah!"
capital all@(x:xs) = "The first letter of '" ++ all ++ "' is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat"
  | otherwise   = "whale"

max' :: (Ord a) => a -> a -> a
--a `max'` b
max' a b
  | a > b = a
  | otherwise = b

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat"
  | otherwise   = "whale"
    where bmi = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
        
--where bindings happen at end of function, visible to whole scope
--let bindings allow binding anywhere and are expressions themselves

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2*pi*r*h
      topArea = pi*r^2
  in sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "no head for empty lists!"
                      (x:_) -> x

descList :: [a] -> String
descList xs = "The list is " ++ case xs of [] -> "empty"
                                           [x] -> "singleton list"
                                           xs -> "a longer list"

--in a recursive function, returning not a recursive call is the 'edge condition'
--you do computations in Haskell by declaring what something is instead of how you get it.

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum' of empty list"
maximum' [x] = x
--maximum' (x:xs) = max x (maximum' xs)
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
    where maxTail = maximum' xs
          
reverse' :: [a] -> [a]          
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x
--take 5 (repeat 3)

zip' :: [a] -> [b] -> [(a,b)]
--my first try
--zip' (xa:xsa) (xb:xsb) = [(xa,xb)] ++ zip' xsa xsb
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <-xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

divTen :: (Floating a) => a -> a
divTen = (/10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

--functions aren't instances of Show typeclass, so don't try to print partially applied functions

appTwice :: (a -> a) -> a -> a
appTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f y x = f x y
flip' f = g
  where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted = quicksort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

--multiple map and filters only pass over list once thanks to laziness

--find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

--find the sum of all odd squares that are smaller than 10,000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

--collatz sequences

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n  = n:chain (n*3 + 1)

--for all numbers between 1..100 how many chains have a length greater than 15?
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

--let x = map (*) [0..]
--(listOfFuns !! 4) 5

numLongChains' = length (filter (\x -> length x > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
--sum' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
--map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

maximumm :: (Ord a) => [a] -> a
maximumm = foldr1 (\x acc -> if x > acc then x else acc)

headd :: [a] -> a
headd = foldr1 (\x _ -> x)

-- 07:42 mauke: @src foldr
-- 07:42 lambdabot: foldr f z []     = z
-- 07:42 lambdabot: foldr f z (x:xs) = f x (foldr f z xs)
-- 07:42 mauke: see that last line?
-- 07:42 mauke: it simply calls f
-- 07:43 brbr: but there's a recursive call on the tail (xs)...
-- 07:43 mauke: not yet
-- 07:43 mauke: your f ignores that argument (_)
-- 07:43 mauke: so it's simply thrown away
-- 07:43 Jafet: @pl \x _ -> x
-- 07:43 lambdabot: const
-- 07:44 Jafet: Though I never expected head = foldr1 const
-- 07:45 brbr: how come it looks like foldr traverses from left-to-right ?
-- 07:46 mauke: because it does
-- 07:46 brbr: @src foldl
-- 07:46 lambdabot: foldl f z []     = z
-- 07:46 lambdabot: foldl f z (x:xs) = foldl f (f z x) xs
-- 07:46 brbr: oh wow
-- 07:46 Jafet: > foldr f 0 [1,2,3,4,5] :: Expr
-- 07:46 lambdabot:  f 1 (f 2 (f 3 (f 4 (f 5 0))))
-- 07:46 Jafet: > foldl f 0 [1,2,3,4,5] :: Expr
-- 07:46 lambdabot:  f (f (f (f (f 0 1) 2) 3) 4) 5
-- 07:46 Jafet: You'll get used to it.

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []
--reverse'' = foldl (flip (:)) []

--module Shapes
--( Point(..)
--, Shape(..)
--, surface
--  ... 
--) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float
           | Rectangle Point Point
           deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


--data Person = Person String String Int Float String String deriving (Show)

--getters
--firstName :: Person -> String
--firstName (Person firstname _ _ _ _ _) = firstname
-- ... this is horrible...

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- we automatically get getters this way.. :t flavor ==> flavor :: Person -> String

-- data Maybe a = Nothing
--              | Just a
--              deriving (Show)

-- 'a' is a type parameter
-- Maybe is a type constructor
-- so we can have: Maybe Int, Maybe String, etc... but never just Maybe

-- Maybe represents an option of either having nothing or having one of something... type doesn't matter

data Day = M | T | W | R | F | S | N deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- minBound :: Day
-- succ M

-- type synonyms:
-- type String = [Char]

--phoneBook :: [(String,String)]
type PhoneBook = [(String,String)]
phoneBook :: PhoneBook
phoneBook =
  [("betty","555-5555")
  ,("joe","333-3333")
  ]

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- used for pattern matching types on left or right
-- say you're interested in how some function failed, use result type
-- of Either a b where a is some type that reveals something about failure
-- and b is type of successful computation. hence errors use Left, results Right

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- search for a code in a locker map.  fail if locker is taken or locker num doesn't exist
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
          [(100,(Taken,"ZD#KS"))
          ,(101,(Free,"K#S(@"))
          ,(102,(Free,"%KSKD"))
          ,(103,(Taken,"DFS@"))
          ]

