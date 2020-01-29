{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.Maybe (fromMaybe)

-- Free Applicative

data Ap f a where
  Pure' :: a -> Ap f a
  Ap   :: f a -> Ap f (a -> b) -> Ap f b

instance Functor f => Functor (Ap f) where
  fmap f (Pure' a) = Pure' $ f a
  fmap f (Ap x y) = Ap x ((f .) <$> y)

instance Functor f => Applicative (Ap f) where
  pure = Pure'
  Pure' f <*> y = f <$> y
  Ap x y <*> z = Ap x (flip <$> y <*> z)
  -- Another variation
  -- x <*> Pure y = ($ y) <$> x
  -- x <*> Ap y z = Ap y ((.) <$> x <*> z)
  -- z `Ap` (y `Ap` (x `Ap` Pure f))

data Arg a =
    Flag String (Bool -> a)
  | Option String (Maybe String -> a)
  deriving Functor

type CommandLine = Ap Arg

flag :: String -> CommandLine Bool
flag s = Ap (Flag s id) (Pure' id)

option :: String -> CommandLine (Maybe String)
option s = Ap (Option s id) (Pure' id)

data Config =
  Config
    { background :: Bool
    , file :: String
    }

readCommandLine :: CommandLine Config
readCommandLine = Config <$> flag "background" <*> (fromMaybe "out" <$> option "file")

argNames :: CommandLine a -> [String]
argNames (Pure' _) = []
argNames (arg `Ap` rest) = argNames rest ++ [name arg]
  where
    name :: Arg a -> String
    name (Flag nm _) = nm
    name (Option nm _) = nm

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

instance Applicative f => Monad (Free f) where
  return = pure -- (!)
  Pure a >>= f = f a
  Free m >>= f = Free $ (>>= f) <$> m
