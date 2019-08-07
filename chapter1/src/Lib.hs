module Lib where

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _)   = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

example :: Tree Integer
example =
  Node
    (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
    (Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 6) (Leaf 7)))

instance Show a => Show (Tree a) where
  show (Leaf n)   = "Leaf " ++ show n
  show (Node l r) = "Node (" ++ show l ++ ") (" ++ show r ++ ")"

relabel' :: Tree a -> Integer -> (Tree (Integer, a), Integer)
relabel' (Leaf x) i = (Leaf (i, x), i + 1)
relabel' (Node l r) i =
  let (l', i1) = relabel' l i
      (r', i2) = relabel' r i1
   in (Node l' r', i2)

relabel :: Tree a -> Tree (Integer, a)
relabel t = fst $ relabel' t 0

--type WithCounter a = Integer -> (a, Integer)
newtype WithCounter a =
  WithCounter
    { evalWithCounter :: Integer -> (a, Integer)
    }

-- next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
-- next f g = \initialCounter -> let (x, nextCounter) = f initialCounter in g x nextCounter
next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
next ma famb =
  WithCounter $ \i ->
    let (x, i') = evalWithCounter ma i
     in evalWithCounter (famb x) i'

pure' :: a -> WithCounter a
pure' x = WithCounter $ (,) x

relabelWithNext :: Tree a -> WithCounter (Tree (Integer, a))
relabelWithNext (Leaf x) = WithCounter $ \i -> (Leaf (i, x), i + 1)
relabelWithNext (Node l r) =
  relabelWithNext l `next` \l' ->
    relabelWithNext r `next` \r' -> pure' $ Node l' r'

newtype State s a =
  State
    { evalState :: s -> (a, s)
    }

-- Exercise 1.1, rewrite next and pure' in terms of State
bind :: State s a -> (a -> State s b) -> State s b
bind sa sasa =
  State $ \s ->
    let (x, s') = evalState sa s
     in evalState (sasa x) s'

pure'' :: a -> State s a
pure'' x = State $ (,) x

-- data [a] = [] | a : [a]
len :: [a] -> Integer
len []    = 0
len (_:t) = 1 + len t

-- hlint suggests this should be `foldr (:) input2 input1`
concatList :: [a] -> [a] -> [a]
concatList [] xs    = xs
concatList (h:t) xs = h : concatList t xs

map' :: (a -> b) -> [a] -> [b]
map' f []    = []
map' f (h:t) = f h : map' f t

concat :: [[a]] -> [a]
concat = foldr (++) []

data Person =
  Person
    { name :: String
    , age  :: Integer
    }

validateName :: String -> Maybe String
validateName = undefined

validateAge :: Integer -> Maybe Integer
validateAge = undefined

validatePerson :: String -> Integer -> Maybe Person
validatePerson name age =
  case validateName name of
    Nothing -> Nothing
    Just name' ->
      case validateAge age of
        Nothing   -> Nothing
        Just age' -> Just $ Person name' age'

then_ :: Maybe a -> (a -> Maybe b) -> Maybe b
then_ Nothing _  = Nothing
then_ (Just a) f = f a

validatePerson' :: String -> Integer -> Maybe Person
validatePerson' name age =
  validateName name `then_` \name' ->
    validateAge age `then_` \age' -> Just $ Person name' age'

-- next  :: State s a -> (a -> State s b) -> State s b
-- then_ :: Maybe   a -> (a -> Maybe   a) -> Maybe   b
flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe (Just (Just x)) = Just x
flattenMaybe _               = Nothing

-- Is it possible to flattenMaybe using only then_?
flattenMaybe' :: Maybe (Maybe a) -> Maybe a
flattenMaybe' mma = mma `then_` id

flattenState :: State s (State s a) -> State s a
flattenState ss = ss `bind` id
-- f :: [a] -> (a -> [b]) -> [b], concatMap
-- class Functor f => Applicative f where
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
--
-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
-- instance Monad Maybe where
--   return = Just
--   (>>=) = then_
--
-- instance Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- \x -> x + 1, (+1)
