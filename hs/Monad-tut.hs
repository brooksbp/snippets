--|-----------------------------------------------------------------------------

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b  -- "bind"
  return :: a -> m a                 -- "return"

-- return a >>= f  <=>  f a

-- m >>= return  <=>  m

-- (m >>= f) >>= g  <=>  (\x -> f x >>= g)

(>>) :: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k

--|-----------------------------------------------------------------------------

-- do { a <- f ; m }  <=>  f >>= \a -> do { m }

-- do { f ; m }  <=>  f >> do { m }

-- do { m }  <=>  m

-- Monad laws re-written in do-notation:

{- do x <- m
      return x
 = do m

   do y <- return x
      f y
 = do f x

   do b <- do a <- m
              f a
      g b
 = do a <- m
      b <- f a
      g b
 = do a <- m
      do b <- f a
         g b
-}

--|-----------------------------------------------------------------------------

data Maybe a = Just a | Nothing

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing  >>= k = Nothing

  return = Just

-- (Just 3) >>= (\x -> return (x + 1))
-- Just 4

-- Nothing >>= (\x -> return (x + 1))
-- Nothing

-- return 4 :: Maybe Int
-- Just 4

--|-----------------------------------------------------------------------------

instance Monad [] where
  m >>= f = concat (map f m)
  return x = [x]

--|-----------------------------------------------------------------------------

-- A unifrom interface for talking about: Failure, Collections, and Effects.

sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q = do
  x <- p
  y <- q
  return (x:y)

-- sequencing a list of Maybe values allows us to collect the results of a
-- series of computations which can possibly fail and yeild the aggregate
-- values only if they all succeed

sequence :: [Maybe a] -> Maybe [a]

sequence [Just 3, Just 4]
Just [3,4]

sequence [Just 3, Just 4, Nothing]
Nothing

-- since the bind operation for the list monad forms the pairwise list of
-- elements, folding the bind over a list of lists implements the general
-- cartesian product for an arbitrary number of lists.

sequence :: [[a]] -> [[a]]

sequence [[1,2,3],[10,20,30]]
[[1,10],[1,20],[1,30],[2,10],[2,20],[1,30],[3,10],[3,20],[3,30]]

-- sequence takes a list of IO actions, performs them sequencially, and returns
-- the list of resulting values in the order sequenced.

sequence :: [IO a] -> IO [a]

sequence [getLine, getLine]
a
b
["a", "b"]
