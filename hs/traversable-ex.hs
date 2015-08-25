{-# LANGUAGE DeriveFoldable #-}
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Control.Applicative
import Control.Monad.Identity (runIdentity)
import Prelude hiding (mapM_, foldr)

data Tree a = Node a [Tree a] deriving (Show)

instance Functor Tree where
  fmap f (Node x ts) = Node (f x) (fmap (fmap f) ts)

instance Traversable Tree where
  traverse f (Node x ts) = Node <$> f x <*> traverse (traverse f) ts

instance Foldable Tree where
  foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts


tree :: Tree Integer
tree = Node 1 [Node 1 [], Node 2 [], Node 3 []]


ex1 :: IO ()
ex1 = mapM_ print tree

ex2 :: Integer
-- ex2 = foldr (+) 0 tree
ex2 = sum tree

ex3 :: Maybe (Tree Integer)
ex3 = traverse (\x -> if x > 2 then Just x else Nothing) tree

ex4 :: Tree Integer
ex4 = runIdentity $ traverse go tree

go :: (Num a, Applicatve f) => a -> f a
go x = pure (x+1)


data BTree a
  = Empty
  | Leaf a
  | BNode (BTree a) a (BTree a)
  deriving (Show, Foldable)

instance Functor BTree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (BNode l k r) = BNode (fmap f l) (f k) (fmap f r)

instance Traversable BTree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (BNode l k r) = BNode <$> traverse f l <*> f k <*> traverse f r
