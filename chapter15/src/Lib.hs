module Lib where

(+++) :: [a] -> [a] -> [a]
[]       +++ ys = ys
(x : xs) +++ ys = x : xs +++ ys

rev :: [a] -> [a]
rev []       = []
rev (x : xs) = rev xs ++ [x]

rev' :: [a] -> [a]
rev' = go []
 where
  go acc []       = acc
  go acc (x : xs) = go (x : acc) xs

revF :: [a] -> [a]
revF = foldl (flip (:)) []

-- 15.1.1 Difference Lists

newtype DList a = DList ([a] -> [a])

toList :: DList a -> [a]
toList (DList dl) = dl []

empty :: DList a
empty = DList id

fromList :: [a] -> DList a
fromList = DList . flip (++)

instance Semigroup (DList a) where
  DList xs <> DList ys = DList (xs . ys)

instance Monoid (DList a) where
  mempty = empty

revInDList :: [a] -> [a]
revInDList = toList . rev'
 where
  rev' []       = mempty
  rev' (x : xs) = rev' xs <> fromList [x]
