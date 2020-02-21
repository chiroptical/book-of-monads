{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- 15.1.2 Church and Scott Encodings

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ _ acc [] = acc
foldr_ f v (x : xs) = f x (foldr_ f v xs)

-- Intuition for foldr is constructor substitution
-- Cons 1 (Cons 2 (Cons 3 Nil))
-- foldr f n [1, 2, 3]
-- Replace Cons with `f` and Nil with `n`
-- i.e.
-- f 1 (f 2 (f 3 n)) 

sum_ :: [Integer] -> Integer
sum_ = foldr_ (+) 0

concat_ :: [a] -> [a] -> [a]
concat_ = foldr_ (:)

newtype EList a = EList (forall b. (a -> b -> b) -> b -> b)

toEList :: [a] -> EList a
toEList xs = EList (\f v -> foldr f v xs)

fromEList :: EList a -> [a]
fromEList (EList fold) = fold (:) []

nil :: EList a
nil = EList (\_ v -> v)

cons :: a -> EList a -> EList a
cons x (EList fold) = EList (\f v -> f x (fold f v))

-- null :: 
null (EList fold) = fold (\_ _ -> False) True

head (EList fold) = fold const (error "empty list")

tail (EList fold) = EList (\f v -> fold (\h t g -> g h (t f))
                                        (\_ -> v) (\_ v' -> v'))

matchList :: (a -> [a] -> b) -> b -> [a] -> b
matchList cons nil [] = nil
matchList cons nil (x : xs) = cons x xs

matchListWithCons :: a -> (a -> [a] -> b) -> b -> [a] -> b
matchListWithCons _ _ n [] = n
matchListWithCons x f _ ys = f x ys

data SList a = SList (forall b. (a -> [a] -> b) -> b -> b)

-- Exercise 15.1: Define `nil,cons,null,head,tail` for Scott encoded lists

nilS :: SList a
nilS = SList (\_ v -> v)

sFromList :: [a] -> SList a
sFromList [] = nilS
sFromList (x : xs) = SList $ \cons _ -> cons x xs

sToList :: forall a. SList a -> [a]
sToList (SList slist) = slist (:) []

nullS :: SList a -> Bool
nullS (SList slist) = slist @Bool cons True
  where
    cons :: a -> [a] -> Bool
    cons _ _ = False

headS :: forall a. SList a -> Maybe a
headS (SList slist) = slist @(Maybe a) cons Nothing
  where
    cons :: a -> [a] -> Maybe a
    cons x _ = Just x

-- tailS :: forall a. SList a -> SList a
-- tailS (SList slist) = SList $ \cons nil -> slist (inCons cons nil) (error "Can't tail an nilS")
--   where
--     inCons cons nil _ [] = nil
--     inCons cons nil _ (x : xs) = cons x xs

tailS :: forall a. SList a -> Maybe (SList a)
tailS (SList slist) = slist (\_ xs -> Just (sFromList xs)) Nothing

consS :: forall a. a -> SList a -> SList a
consS x (SList slist) = SList $ \cons nil -> slist (consCons cons) (cons x [])
  where
    consCons cons y [] = cons x [y]
    consCons cons y ys = cons x (y : ys)
