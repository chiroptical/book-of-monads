-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Writer where

import           Data.Coerce                    ( coerce )
import Data.Semigroup

newtype Writer w a =
  Writer
    { runWriter :: (w, a)
    }
  -- deriving (Functor, Applicative, Monad)

instance Functor (Writer w) where
  fmap f wa = Writer $ f <$> runWriter wa

instance Monoid w => Applicative (Writer w) where
  pure = Writer . (mempty ,)
  -- Writer (w, f) <*> Writer (w', a) = Writer (w <> w', f a)
  wf <*> wa = Writer $ runWriter wf <*> runWriter wa

instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (w, a) >>= f =
    let Writer (w', b) = f a
     in Writer (w <> w', b)

tell :: w -> Writer w ()
tell = Writer . (, ())

example :: Writer (Sum Int) String
example = do
  tell (Sum 3)
  tell (Sum 4)
  return "seven" -- Writer (Sum 0, "seven")
  -- Writer (Sum 3 <> Sum 4 <> Sum 0, "seven")

data Expr =
    Lit Float
  | Add Expr Expr
  | Divide Expr Expr

eval :: Expr -> Float
eval (Lit n) = n
eval (Add e g) = eval e + eval g
eval (Divide e g) =
  case (eval e, eval g) of
    (_, 0) -> 0
    (u, v) -> u / v

eval' :: Expr -> Writer [String] Float
eval' (Lit n) = pure n
eval' (Add e g) = (+) <$> eval' e <*> eval' g
eval' (Divide e g) = do
  x <- eval' e
  y <- eval' g
  case (x, y) of
    (_, 0) -> do
      tell ["Divide by zero!"]
      return 0
    (u, v) -> pure $ u / v

  
