-- Basically, when completing Chapter 9
-- this was still difficult. Going to practice
-- occasionally but this has nothing to do with
-- Chapter 10...
module Cont where

newtype Cont r a =
  Cont
    { runCont :: (a -> r) -> r
    }

instance Functor (Cont r) where
  fmap f (Cont arr) = Cont $ \cb -> arr (cb . f)

instance Applicative (Cont r) where
  pure a = Cont $ \cb -> cb a
  Cont farr <*> Cont arr = Cont $ \cb -> farr (\cb' -> arr (cb . cb'))

instance Monad (Cont r) where
  return = pure
  -- runCont (afbrr a) :: (b -> r) -> r
  -- runCont (afbrr a) cb :: r
  -- arr :: (a -> r) -> r
  Cont arr >>= afbrr = Cont $ \cb -> arr (\a -> runCont (afbrr a) cb)
