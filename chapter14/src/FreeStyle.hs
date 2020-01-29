{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module FreeStyle where

import           Free

data FSF r =
    WriteFile FilePath String (Either IOError () -> r)
  | ReadFile FilePath (Either IOError String -> r)
  deriving Functor

data RandomGenF r = Random Int Int deriving Functor

data FSRandomF r = FSF r :+: RandomGenF r

-- Free r, r :k: * -> * (:k: reads "with kind")
-- type FSRandom = Free FSRandomF

type FSRandom = Free (FSF :+: RandomGenF)

data (f :+: g) a = InL (f a) | InR (g a)

-- Exercise 14.1, show that Sum f g is a Functor if both f and g are Functors
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InR fa) = InR $ f <$> fa
  fmap f (InL fa) = InL $ f <$> fa

-- Interpretation will take the following shape, where {M} is the Monad to transform to
-- interpret = foldFree interpret'
--   where
--     interpret' :: (f :+: g) a -> {M} a
--     interpret' = _a

combine :: (f a -> m a) -> (g a -> m a) -> (f :+: g) a -> m a
combine f _ (InL x) = f x
combine _ g (InR x) = g x

writeFile :: FilePath -> String -> FSRandom (Either IOError ())
writeFile path contents = liftF (InL $ WriteFile path contents id)

readFile :: FilePath -> FSRandom (Either IOError String)
readFile path = liftF (InL $ ReadFile path id)

random :: Int -> Int -> FSRandom Int
random x y = liftF (InR $ Random x y)

class f :<: g where
  inject :: f a -> g a

instance f :<: f where
  inject = id

instance f :<: (f :+: g) where
  inject = InL

instance (f :<: h) => f :<: (g :+: h) where
  inject = InR . inject

writeFile_ :: (Functor f, FSF :<: f) => FilePath -> String -> Free f (Either IOError ())
writeFile_ path contents = liftF (inject $ WriteFile path contents id)

readFile_ :: (Functor f, FSF :<: f) => FilePath -> Free f (Either IOError String)
readFile_ path = liftF (inject $ ReadFile path id)

random_ :: (Functor f, RandomGenF :<: f) => Int -> Int -> Free f Int
random_ x y = liftF (inject $ Random x y)

randomWrite :: (Functor f, FSF :<: f, RandomGenF :<: f) => FilePath -> Free f (Either IOError ())
randomWrite path = do
  number <- random_ 1 100
  writeFile_ path (show number)
