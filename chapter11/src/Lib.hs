module Lib where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import           Data.Maybe                     ( isJust )
import Control.Applicative (empty)

data Expr
  = Literal Integer
  | Var String
  | Op Op Expr Expr

data Op
  = Add
  | Subtract
  | Multiply
  | Divide

newtype Assignment =
  Assignment
    { unAssignment :: [(String, Integer)]
    }

newtype Evaluator a =
  Evaluator
    { unEvaluator :: Reader Assignment (Maybe a)
    }

newtype Evaluator2 a =
  Evaluator2
    { unEvaluator2 :: State Assignment (Maybe a)
    }

-- Exercise 11.1, rewrite eval from Chapter 10 using
-- MaybeT
eval :: Expr -> MaybeT (State Assignment) Integer
eval (Literal n) = pure n
eval (Var v) = MaybeT $ gets (lookup v . unAssignment)
eval (Op o x y) = do
  u <- eval x
  v <- eval y
  case o of
    Add -> pure (u + v)
    Subtract -> pure (u - v)
    Multiply -> pure (u * v)
    Divide -> do
      guard $ v /= 0
      pure $ u `div` v

-- To get the integer, you need to

result :: Expr -> Assignment -> (Maybe Integer, Assignment)
result e = runState . runMaybeT $ eval e
