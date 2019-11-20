{-# LANGUAGE LambdaCase #-}

module MaybeT where

-- Exercise 11.2 - Write the Monad instances for
-- transformers with arbitrary monads

import           Data.Maybe                     ( isJust )
import           Control.Monad                  ( guard )
newtype MaybeT m a =
  MaybeT
    { runMaybeT :: m (Maybe a)
    }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ fmap f <$> ma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT f <*> MaybeT a = MaybeT $ (<*>) <$> f <*> a

instance Monad m => Monad (MaybeT m) where
  MaybeT ma >>= f = MaybeT $
    ma >>= \case
      Nothing -> pure Nothing
      Just x -> runMaybeT $ f x
