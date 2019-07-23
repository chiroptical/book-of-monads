module Lib where

data Tree a = Leaf a | Node (Tree a) (Tree a)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

a :: Tree Integer
a = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 6) (Leaf 7)))

instance Show a => Show (Tree a) where
  show (Leaf n) = "Leaf " ++ show n
  show (Node l r) = "Node (" ++ show l ++ ") (" ++ show r ++ ")"
