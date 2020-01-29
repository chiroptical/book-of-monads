{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Inspecting where

import RPNInFreer (Freer (..), RPNInstruction, IStack (..), push, pop)

data LastOp = Return | LastPop | LastPush Integer

class MonadIStack m where
  pop' :: m Integer
  push' :: Integer -> m ()

newtype WithContext c m a =
  C
    { unC :: c -> m (a, c)
    }

-- Exercise 13.19 -- Implement Functor and Applicative for `WithContext LastOp m`
instance Monad m => Functor (WithContext LastOp m) where
  fmap f x = C $ \context -> do
    (x', context') <- unC x context
    pure (f x', context')

instance Monad m => Applicative (WithContext LastOp m) where
  pure x = C $ \_ -> return (x, Return)
  f <*> x = C $ \context -> do
    (f', context') <- unC f context
    (x', context_) <- unC x context'
    pure (f' x', context_)

instance Monad m => Monad (WithContext LastOp m) where
  C x >>= f = C $ \context -> do
    (x', context') <- x context
    unC (f x') context'

instance (Monad m, MonadIStack m) => MonadIStack (WithContext LastOp m) where
  pop' = C $ \case
                LastPush n -> return (n, Return)
                _ -> (, LastPop) <$> pop'
  push' v = C $ \_ -> (, LastPush v) <$> push' v

optimize :: (Monad m, MonadIStack m) => WithContext LastOp m a -> m a
optimize p = fst <$> unC p Return

-- Exercise 13.20 - Convince yourself the above code works
