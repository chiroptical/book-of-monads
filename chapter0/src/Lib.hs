module Lib where

import           Prelude hiding (Eq (..))

class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _ = False

-- Exercise 0.2: Write the Eq instance for (a, b)
instance (Eq a, Eq b) => Eq (a, b) where
  (x, y) == (x', y') = x == x' && y == y'

class Container c where
  empty :: c a
  insert :: a -> c a -> c a

instance Container [] where
  empty = []
  insert = (:)

newtype Queue a =
  Queue
    { unQueue :: [a]
    }

instance Container Queue where
  empty = Queue []
  --insert x (Queue xs) = Queue $ xs ++ [x]
  insert x xs = Queue $ unQueue xs ++ [x]
