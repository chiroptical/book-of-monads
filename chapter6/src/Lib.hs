{-# LANGUAGE TupleSections #-}

module Lib where

-- Exercise 6.1: Implement Functor, Applicative, and Monad
-- for State

newtype State s a =
  State
    { runState :: s -> (a, s)
    }

-- ---------------------------------------------------------------------
instance Functor (State s) where
  fmap f (State sas) = State $ \s -> let (a, s') = sas s in (f a, s')

instance Applicative (State s) where
  -- fab :: s -> (a -> b, s)
  -- sas :: s -> (a, s)
  pure x = State (x, )
  State fab <*> State sas = State $ \s0 ->
    let (f, s1) = fab s0
        (x, s2) = sas s1
    in  (f x, s2)

instance Monad (State s) where
  return = pure
  State sas >>= fasbs =
    State $ \s0 -> let (a, s1) = sas s0 in runState (fasbs a) s1
-- ---------------------------------------------------------------------

-- twitch.tv/WorldSEnder pointed out there is a neat result from category
-- theory ("adjoint functor pair") to simplify the implementation of Functor,
-- Applicative, and Monad. The remainder of this code is used to implement
-- that concept. Above inside the block is the naive solution.

--newtype App s a =
--  App
--    { unApp :: s -> a
--    }

--newtype Tup s a =
--  Tup
--    { unTup :: (a, s)
--    }

--instance Functor (App s) where
--  fmap f (App sas) = App $ f . sas

--instance Functor (Tup s) where
--  fmap f (Tup (a, s)) = Tup (f a, s)

--isoSAT :: State s a -> App s (Tup s a)
--isoSAT (State sas) = Tup <$> App sas

--isoATS :: App s (Tup s a) -> State s a
--isoATS at = State . unApp $ unTup <$> at

--instance Functor (State s) where
--  fmap f = isoATS . (fmap . fmap) f . isoSAT

---- ~ a -> App (Tup a)
---- a -> s -> (a, s) ~ (,)

---- 
--apply :: (s -> a, s) -> a
--apply (f, s) = f s

--applyTup :: Tup s (App s a) -> a
--applyTup = apply . unTup . fmap unApp

---- applyTup' :: App s (Tup s a) -> a
---- applyTup' = apply . unApp . fmap unTup

--unitTuple :: s -> ((), s)
--unitTuple = ((), )

---- Here, the order which we get `a` and `b` is arbitrary
---- Applicative does not guarentee order, however Monad does
--groupState :: (s -> (a, s)) -> (s -> (b, s)) -> (s -> ((a, b), s))
--groupState sas sbs s =
--  let (a, s0) = sas s
--      (b, s1) = sbs s0
--   in ((a, b), s1)

--instance Applicative (State s) where
--  -- (<$) is essentially const with a functorial context
--  pure x = x <$ State unitTuple
--  State fsas <*> State sas = apply <$> (State $ groupState fsas sas)

--instance Monad (State s) where
--  return = pure
--  -- sas :: State s a
--  -- fas :: a -> State s b
--  -- fmap (isoSAT . fas) sas :: State s (App s (Tup s b))
--  -- isoSAT ^ :: App s (Tup s (App s (Tup s b)))
--  -- fmap applyTup ^ :: App s (Tup s b)
--  -- isoATS ^ :: State s b
--  sas >>= fas = isoATS $ fmap applyTup $ isoSAT $ fmap (isoSAT . fas) sas
