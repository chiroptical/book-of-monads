{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Maybe                     ( isJust )
import           Control.Applicative
import           Data.Functor.Identity

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
eval (Var     v) = MaybeT $ gets (lookup v . unAssignment)
eval (Op o x y ) = do
  u <- eval x
  v <- eval y
  case o of
    Add      -> pure (u + v)
    Subtract -> pure (u - v)
    Multiply -> pure (u * v)
    Divide   -> do
      guard $ v /= 0
      pure $ u `div` v

-- To get the integer, you need to

result :: Expr -> Assignment -> (Maybe Integer, Assignment)
result e = runState . runMaybeT $ eval e

-- Exercise 11.3 Check that `T` and `Identity T` are isomorphic for any
-- type `T`

toIdentity :: a -> Identity a
toIdentity = pure

fromIdentity :: Identity a -> a
fromIdentity (Identity x) = x

-- Exercise 11.4 - prove that MaybeT and Maybe form the same Functor & Monad.
-- i.e. Prove MaybeT Identity a is isomorphic w/ Maybe

-- Need to keep in mind, the data constructor
-- MaybeT :: Identity (Maybe a)
toMaybeT :: Maybe a -> MaybeT Identity a
toMaybeT = \case
  Just x -> return x
  Nothing -> MaybeT $ return Nothing

fromMaybeT :: MaybeT Identity a -> Maybe a
-- fromMaybeT (MaybeT ima) = fromIdentity ima
-- fromMaybeT (MaybeT (Identity ma)) = ma
fromMaybeT = runIdentity . runMaybeT

type Environment = String
type Message = String
type Person = String

newtype Stack a = Stack
  { runStack :: ReaderT Environment (WriterT [Message] (MaybeT Identity)) a
  }

-- ReaderT Environment (WriterT [Message] Maybe) Person
-- ~ Environment -> (WriterT [Message] Maybe) Person
-- ~ Environment -> Maybe (Person, [Message])
-- /~ Reader Environment (Writer [Message] (Maybe Person))
-- the final one is: Environment -> (Maybe Person, [Message])

-- Parsing for Free!

newtype Parser a =
  Parser
    { runParser :: StateT String [] a
    } deriving (Functor, Applicative, Monad, Alternative)

-- ~ String -> [(a, String)]

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = Parser . StateT $
  \case
    "" -> []
    -- (c : cs) | p c -> [(c, cs)]
    --          | otherwise -> []
    (c : cs) -> [(c, cs) | p c]

char :: Char -> Parser Char
char = satisfies . (==)

-- Exercise 11.5 use `char` as the primitive function
-- implement `satisfies` in terms of `char`
-- char' :: Char -> Parser Char
-- char' c = Parser . StateT $
--   \case
--     "" -> []
--     (x : xs) -> [(x, xs) | x == c]

-- satisfies' :: (Char -> Bool) -> Parser Char
-- satisfies' p = Parser . StateT $
--   \case
--     "" -> []
--     (c : cs) -> do
--       char' c
--       _a

end :: Parser ()
end = Parser . StateT $
  \case
    "" -> [((), "")]
    _ -> []

hex :: Parser String
hex = (\a b c d -> [a, b, c, d])
  <$ char '0' <* (char 'x' <|> char 'X')
  <*> hexdigit <*> hexdigit <*> hexdigit <*> hexdigit
    where 
      hexdigit = satisfies (`elem` "abcdefABCDEF0123456789")
