{-# LANGUAGE GADTs #-}
-- Generalized Algebraic Datatypes

-- A generalization of Algebraic Data Types.
-- They allow you to state the type of the constructors.

-- GADTs mainly used to implement DSLs.


data Expr = I Int           -- integer constants
          | B Bool          -- boolean constants
          | Add Expr Expr   -- add two expressions
          | Mul Expr Expr   -- multiply two expressions
          | Eq  Expr Expr   -- equality test

-- (5+1)*7    ==>  (I 5 `Add` I 1) `Mul` I 7 :: Expr
--  5+1 == 7  ==>  (I 5 `Add` I 1) `Eq` I 7

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just (Left n)
eval (B b) = Just (Right b)
eval (Add a@(I _) b@(I _)) =
  case (eval a, eval b) of (Just (Left n1), Just (Left n2)) -> Just (Left $ n1 + n2)
                           (_,_)                            -> Nothing
--eval (Mul (I a) (I b)) = Just (Left (eval a * eval b))

-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- these definitions don't make sense for adding a bool to an int...
-- so eval may return a bool or int, but may also fail because no way to eval expr,
-- so use Maybe (Either Int Bool).

-- just took quite a while to implement (Add _ _) for Maybe (Eit.. type definition

-- How about have Haskell's type system rule out invalid expressions?
-- Avoid checking types while deconstructing the abstract syntax tree.


--- Phantom types

data Exp a = Ii Int
           | Bb Bool
           | Addd (Exp a) (Exp a)
           | Mull (Exp a) (Exp a)
           | Eqq  (Exp a) (Exp a)
           deriving (Show)

-- 'a' is the phantom type which tracks the type of the expression (Exp)

-- Instead of Add :: Exp a -> Exp a -> Exp a only provide smart constructors
-- with a more restricted type

add :: Exp Int -> Exp Int -> Exp Int
add = Addd

i :: Int -> Exp Int
i = Ii

b :: Bool -> Exp Bool
b = Bb


-- eval' :: Exp a -> a
-- eval' (Ii n) = n
-- ... this doesn't work, bc can't ensure constructed with i :: Int -> Exp Int


data Ex a where
  Iii   :: Int  -> Ex Int
  Bbb   :: Bool -> Ex Bool
  Adddd :: Ex Int -> Ex Int -> Ex Int
  Mulll :: Ex Int -> Ex Int -> Ex Int
  Eqqq  :: Ex Int -> Ex Int -> Ex Bool

eval'' :: Ex a -> a
eval'' (Iii n) = n        -- compiler infers a :: Int, legal to return n :: Int
eval'' (Bbb b) = b
eval'' (Adddd e1 e2) = eval'' e1 + eval'' e2
eval'' (Mulll e1 e2) = eval'' e1 * eval'' e2
eval'' (Eqqq  e1 e2) = eval'' e1 == eval'' e2


-- data Maybe a = Nothing                    :: Maybe a
--              | Just a                     :: a -> Maybe a
-- data List a = Nil                         :: List a
--             | Cons a (List a)             :: a -> List a -> List a
-- data RoseTree a = RoseTree a [RoseTree a] :: a -> [RoseTree a] -> RoseTree a

-- data Maybe a where
--   Nothing :: Maybe a
--   Just    :: a -> Maybe a
-- data List a where
--   Nil  :: List a
--   Cons :: a -> List a -> List a
-- data RoseTree a where
--   RoseTree :: a -> [RoseTree a] -> RoseTree a


myvar = 4