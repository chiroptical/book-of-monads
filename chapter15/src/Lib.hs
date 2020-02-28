{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Lib where

import Free

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- Exercise 15.2 Tree can be written as a free monad. What is the pattern Functor? To which constructor in Free does Leaf correspond?

-- Leaf corresponds to the `Pure` constructor

type TreeF = Free Tree

instance Applicative Tree where
  pure = Leaf 
  Leaf f <*> t = fmap f t
  Node l r <*> t = Node (l <*> t) (r <*> t)

instance Monad Tree where
  Leaf x >>= f = f x
  Node l r >>= f = Node (l >>= f) (r >>= f)

fullTree :: Integer -> Tree Integer
fullTree 0 = Leaf 0
fullTree n = do
  fullTree (n - 1)
  Node (Leaf n) (Leaf n)

-- 15.2.1 Codensity

newtype Codensity m a =
  Codensity
    { runCodensity :: forall b. (a -> m b) -> m b
    }

instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity $ \k -> m (k . f)

instance Applicative (Codensity k) where
  pure x = Codensity $ \k -> k x
  Codensity f <*> Codensity x = Codensity $ \bfr -> f (\ab -> x (bfr . ab))

instance Monad (Codensity k) where
  Codensity x >>= f = Codensity $ \k -> x (\y -> runCodensity (f y) k)

lower :: Monad m => Codensity m a -> m a
lower (Codensity k) = k pure

lift :: Monad m => m a -> Codensity m a
lift m = Codensity (m >>=)

class MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

instance Functor f => MonadFree f (Free f) where
  wrap = Free

instance (Functor f, MonadFree f m) => MonadFree f (Codensity m) where
  wrap t = Codensity $ \h -> wrap (fmap (`runCodensity` h) t)

improve :: Functor f => (forall m. MonadFree f m => m a) -> Free f a
improve = lower

liftF :: (Functor f, Monad m, MonadFree f m) => f a -> m a
liftF = wrap . fmap pure

-- Section 15.2.2 is where we stopped. This is well over my head and I need to
-- sit down with a pen and paper and try to fully comprehend what is going on
