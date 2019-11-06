{-# LANGUAGE LambdaCase #-}

module Eval where

import           Control.Monad              ((>=>))
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity

type Name = String

data Expr
  = Literal Integer
  | Var Name
  | Op Op Expr Expr

data Op
  = Add
  | Subtract
  | Multiply
  | Divide

type Assignment = [(Name, Integer)]

eval :: Expr -> Reader Assignment (Maybe Integer)
eval (Literal n) = return $ Just n
eval (Var v) = lookup v <$> ask
eval (Op o x y) = do
  u <- eval x
  v <- eval y
  case (u, v) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just u', Just v') ->
      return $
      case o of
        Add -> Just (u' + v')
        Subtract -> Just (u' - v')
        Multiply -> Just (u' * v')
        Divide ->
          if v' == 0
            then Nothing
            else Just (u' `div` v')

newtype Evaluator a =
  Evaluator
    { runEvaluator :: ReaderT Assignment Identity (Maybe a)
    }

instance Functor Evaluator where
  fmap f (Evaluator e) =
    Evaluator . ReaderT $
    runReaderT e >=> \case
      Nothing -> return Nothing
      Just y -> return . Just $ f y

instance Applicative Evaluator where
  pure x = Evaluator . return $ Just x
  Evaluator f <*> Evaluator e =
    Evaluator . ReaderT $ \a ->
      runReaderT f a >>= \case
        Nothing -> return Nothing
        Just f' ->
          (runReaderT e >=> \case
             Nothing -> return Nothing
             Just x -> return . Just $ f' x)
            a

instance Monad Evaluator where
  fail _ = Evaluator $ return Nothing
  Evaluator e >>= f =
    Evaluator . ReaderT $ \a ->
      runReaderT e a >>= \case
        Nothing -> return Nothing
        Just y ->
          let Evaluator e' = f y
           in runReaderT e' a

eval' :: Expr -> Evaluator Integer
eval' (Literal n) = return n
eval' (Var v) = do
  a <- Evaluator $ Just <$> ask
  case lookup v a of
    Nothing -> fail $ v ++ " is not in the assignments"
    Just v' -> return v'
eval' (Op o x y) = do
  u <- eval' x
  v <- eval' y
  case o of
    Add -> return $ u + v
    Subtract -> return $ u - v
    Multiply -> return $ u * v
    Divide ->
      if v == 0
        then fail "Divisor can't be zero!"
        else return $ u `div` v
