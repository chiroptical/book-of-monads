module Lib where

-- a -> m b
-- 
-- > map (+1) [1, 2, 3]
-- [2, 3, 4]

import           Control.Monad                  ( forM )
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ []       = return []
mapM' f (x : xs) = do
  r  <- f x
  rs <- mapM' f xs
  return (r : rs)

mapM'' f (x : xs) = (:) <$> f x <*> mapM'' f xs

m = forM ["Alejandro", "Elena"] $ \name -> print ("Hello " ++ name)

sequenceL :: Monad m => [m a] -> m [a]
sequenceL []       = return []
sequenceL (x : xs) = (:) <$> x <*> sequenceL xs

-- In the above, hlint suggests the following change

sequenceL' :: Monad m => [m a] -> m [a]
sequenceL' = foldr (\x -> (<*>) ((:) <$> x)) (return [])

mapM''' f = sequenceL' . map f

-- Exercise 4.1, write implementations of filterM, zipWithM,
-- and replicateM using their pure counterparts composed with
-- sequence

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] _  = return []
zipWithM _ _  [] = return []
-- zipWith f [x] [y] :: [m c]
-- m [c] <> m [c], (++) <$> m [c] :: m ((++) [c]), then apply
zipWithM f (x : xs) (y : ys) =
  (++) <$> (sequence $ zipWith f [x] [y]) <*> zipWithM f xs ys

-- After working with replicateM and filterM, it became
-- obvious that the above implementation is overly complicated
-- zipWith f xs ys :: [m c]

zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f xs ys = sequence $ zipWith f xs ys

replicateM :: Monad m => Int -> m a -> m [a]
--replicateM n ma = replicate n <$> ma
replicateM n ma = ma >> sequence (replicate n ma)

-- Something is wrong with this one? What is it?
-- -> One can't extract a function (a -> Bool) from (a -> m Bool)
-- -> must be a recursive definition
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f []       = return []
filterM f (x : xs) = do
  b <- f x
  if b
   then (:) <$> return x <*> filterM f xs
   else filterM f xs
