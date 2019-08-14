module Lib where

plus :: (Num a, Monad m) => m a -> m a -> m a
plus mx my = do
  a <- mx
  b <- my
  return (a + b)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

a :: Maybe Int
a = Just 1

b :: Maybe Int
b = Just 2

c :: Maybe Int
c = plus a b

g :: a -> b -> c
g = undefined

-- fmap g ma :: m (b -> c)
-- m (b -> c) -> m b -> m c

-- Exercise 3.1. Write the implementation of ap
ap :: Monad m => m (b -> c) -> m b -> m c
ap mf mb = do
  f <- mf
  b <- mb
  return $ f b

-- f :: a -> b -> c -> d
-- x :: m a
-- y :: m b
-- z :: m c
-- fmap f x `ap` y `ap` z
lift3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f x y z = fmap f x `ap` y `ap` z

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- Exercise 3.2: Implement fmap in terms of `pure` and `(<*>)`
fmap' :: Applicative f => (a -> b) -> f a -> f b
fmap' f fx = pure f <*> fx

newtype ZipList a =
  ZipList
    { getZipList :: [a]
    } deriving Show

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ f <$> xs

instance Applicative ZipList where
  pure x = ZipList $ repeat x
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

-- Copied for illustration purposes in the REPL
data Person =
  Person
    { personName :: String
    , personAge  :: Int
    }

validateName :: String -> Maybe String
validateName = undefined

validateAge :: Int -> Maybe Int
validateAge = undefined

validatePerson :: String -> Int -> Maybe Person
validatePerson name age =
  validateName name >>= \name' ->
    validateAge age >>= \age' -> return $ Person name' age'

-- Exercise 3.4
tripleNest :: (a, b, c) -> (a, (b, c))
tripleNest (a, b, c) = (a, (b, c))

quadNest :: (a, b, c, d) -> (a, (b, (c, d)))
quadNest (a, b, c, d) = (a, (b, (c, d)))

class Functor f => Monoidal f where
  unit :: f ()
  (>*<) :: f a -> f b -> f (a, b)

pure' :: (Monoidal f, Functor f) => a -> f a
pure' x = fmap (const x) unit

ap' :: (Monoidal f, Functor f) => f (a -> b) -> f a -> f b
-- f >*< x ~ f ((a -> b), a)
ap' f x = fmap (\(g, y) -> g y) (f >*< x)

-- Exercise 3.5, define unit and (>*<) in terms of fmap, pure, and (<*>)
unit' :: Applicative f => f ()
unit' = pure ()

(>*>) :: Applicative f => f a -> f b -> f (a, b)
-- (,) <$> fa ~ f ((,) a)
fa >*> fb = (,) <$> fa <*> fb
