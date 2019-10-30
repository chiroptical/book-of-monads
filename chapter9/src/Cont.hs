module Cont where

-- This is simply implementing code from Lib.hs
-- a second time for practice.

newtype Cont r a =
  Cont
    { runCont :: (a -> r) -> r
    }

instance Functor (Cont r) where
  -- f :: a -> b
  -- arr :: (a -> r) -> r
  -- c :: (b -> r)
  fmap f (Cont arr) = Cont $ \c -> arr (c . f)

instance Applicative (Cont r) where
  pure a = Cont $ \c -> c a
  -- abrr :: ((a -> b) -> r) -> r
  -- arr :: (a -> r) -> r
  -- c :: b -> r
  -- d :: a -> b
  Cont abrr <*> Cont arr = Cont $ \c -> abrr $ \d -> arr (c . d)

instance Monad (Cont r) where
  return = pure
  -- arr :: (a -> r) -> r
  -- afbrr :: a -> Cont ((b -> r) -> r)
  -- c :: b -> r
  Cont arr >>= afbrr = Cont $ \c -> arr (\a -> runCont (afbrr a) c)
