module Free where

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free g) = Free $ fmap (f <$>) g

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x@(Pure _) = f <$> x
  Pure f <*> x@(Free _) = f <$> x
  -- f :: Free f (a -> b)
  -- <*> x :: Free f (a -> b) -> Free f b
  Free f <*> x          = Free $ fmap (<*> x) f

instance Functor f => Monad (Free f) where
  Pure x >>= f = f x
  Free x >>= f = Free $ fmap (>>= f) x

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return
