module Lib where

import           Control.Applicative
import           Control.Monad
import Control.Monad.Logic

-- (<|>) :: Maybe a -> Maybe a -> Maybe a
-- Just x  <|> _     = Just x
-- Nothing <|> other = other

-- (<||>) :: Semigroup a => Maybe a -> Maybe a -> Maybe a
-- Just x <||> Just y = Just $ x <> y
-- ...

-- validateName :: String -> Maybe Name
-- validateName s =
--   validateNameEnglish s <|> validateNameSpanish s <|> validateNameDutch s

-- Exercise 7.1, write Functor, Applicative, Monad for Either

data Either' e r = Left' e | Right' r

instance Functor (Either' e) where
  fmap f (Right' x) = Right' (f x)
  fmap _ (Left'  e) = Left' e

instance Applicative (Either' e) where
  pure = Right'
  Right' f <*> Right' x = Right' $ f x
  _        <*> Left'  e = Left' e

instance Monad (Either' e) where
  return = pure
  Right' x >>= f = f x
  Left'  e >>= _ = Left' e

-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

-- class Monad m => MonadPlus m where
--   mzero :: m a
--   mplus :: m a -> m a -> m a

-- Exercise 7.2, write Alternative for Either

instance Monoid e => Alternative (Either' e) where
  empty = Left' mempty
  Right' x <|> _         = Right' x
  Left'  _ <|> Right' x  = Right' x
  Left'  e <|> Left'  e' = Left' $ e <> e'

-- validateAge :: String -> Maybe Age
-- validateAge s = do
--   n <- readMay s
--   if n >= 18
--      then Just n
--      else Nothing

-- validateAge' s = do
--   n <- readMay s
--   guard (n >= 18)
--   Just n

asum :: (Traversable t, Alternative m) => t (m a) -> m a
asum = foldr (<|>) empty

msum :: (Traversable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p x = do
  x' <- x
  guard $ p x'
  return x'
-- mfilter p x = x >>= liftA2 (>>) (guard . p) return

type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quipue", "John", "Mary", "Tom"]

parentChildRelationships :: [(Person, Person)]
parentChildRelationships =
  [ ("Alejandro", "Quipue")
  , ("Elena"    , "Quipue")
  , ("John"     , "Mary")
  , ("John"     , "Tom")
  , ("Mary"     , "Tim")
  ]

grandparentGrandChildRelationships :: [(Person, Person)]
grandparentGrandChildRelationships = do
  (grandp , parent) <- parentChildRelationships
  (parent', grandc) <- parentChildRelationships
  guard (parent == parent')
  return (grandp, grandc)

-- Exercise 7.3, define `siblingRelationships` to
-- return one tuple for every two people who are siblings
-- i.e. they share the same parent 

siblingRelationships :: [(Person, Person)]
siblingRelationships = do
  (parent , child ) <- parentChildRelationships
  (parent', child') <- parentChildRelationships
  guard (parent == parent' && child /= child')
  return (child, child')

sums :: [Integer] -> [(Integer, Integer, Integer)]
sums ns = do
  x <- ns
  y <- ns
  z <- ns
  guard (x + y == z)
  return (x, y, z)

pyts :: [Integer] -> [(Integer, Integer, Integer)]
pyts ns = do
  x <- ns
  y <- ns
  z <- ns
  guard (x ^ 3 + y ^ 3 == z ^ 3)
  return (x, y, z)

triples = liftA2 (<|>) sums pyts

list :: [a] -> Logic a
list = asum . map return

-- Not quite right, but type checks
-- sums' :: [Integer] -> Logic (Integer, Integer, Integer)
-- sums' ns =
--   list ns >>= \x ->
--   list ns >>= \y ->
--   list ns >>= \z ->
--       guard (x + y == z) >> return (x, y, z)

fairTriples :: [Integer] -> Logic (Integer, Integer, Integer)
fairTriples ns = 
  list ns >>- \x ->
  list ns >>- \y ->
  list ns >>- \z ->
      return (x, y, z)

sums' :: [Integer] -> Logic (Integer, Integer, Integer)
sums' ns =
  fairTriples ns >>= \(x, y, z) ->
    guard (x + y == z) >> return (x, y, z)

pyts' :: [Integer] -> Logic (Integer, Integer, Integer)
pyts' ns =
  fairTriples ns >>= \(x, y, z) ->
    guard (x ^ 3 + y ^ 3 == z ^ 3) >> return (x, y, z)

triples' = liftA2 (<|>) sums' pyts'
