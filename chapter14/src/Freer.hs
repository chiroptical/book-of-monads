{-# LANGUAGE GADTs #-}

module Freer where

import           Control.Monad                  ( (<=<) )

data Freer instr a where
  Pure ::a -> Freer instr a
  Impure ::instr a -> (a -> Freer instr b) -> Freer instr b

instance Functor (Freer instr) where
  fmap f (Pure x    ) = Pure $ f x
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (Freer instr) where
  pure = Pure
  Pure f     <*> Pure x     = Pure $ f x
  Impure x k <*> f          = Impure x (\a -> k a <*> f)
  Pure f     <*> Impure x k = Impure x (fmap f . k)

instance Monad (Freer instr) where
  Pure x     >>= f = f x
  Impure x k >>= f = Impure x (f <=< k)

