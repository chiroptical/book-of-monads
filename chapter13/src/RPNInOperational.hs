-- Exercise 13.18 Rewrite IStack from `RPNInFree.hs` using operational style
--
{-# LANGUAGE GADTs #-}

module RPNInOperational where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )

data IStack r where
  Pop ::IStack Integer
  Push ::Integer -> IStack ()
  Pure ::r -> IStack r
  Bind ::IStack r -> (r -> IStack s) -> IStack s

instance Functor IStack where
  fmap = (=<<) . (pure .)

instance Applicative IStack where
  pure  = Pure
  (<*>) = (. flip fmap) . (>>=)

instance Monad IStack where
  (>>=) = Bind

data RPNInstruction = Number Integer | Plus | Times | Subtract deriving Show

interpret :: IStack r -> StateT [Integer] Maybe r
interpret (Pure x  ) = return x
interpret (Bind x f) = interpret x >>= interpret . f
interpret Pop        = do
  p <- pop <$> get
  case p of
    Just (xs, v) -> do
      put xs
      pure v
    Nothing -> lift Nothing
 where
  pop :: [Integer] -> Maybe ([Integer], Integer)
  pop [] = Nothing
  pop xs = Just (tail xs, head xs)
interpret (Push x) = modify ((:) x)

evaluate :: [RPNInstruction] -> IStack Integer
evaluate []               = Pop
evaluate ((Number n) : r) = Push n >> evaluate r
evaluate (Plus       : r) = ((+) <$> Pop <*> Pop) >>= Push >> evaluate r
evaluate (Times      : r) = ((*) <$> Pop <*> Pop) >>= Push >> evaluate r
evaluate (Subtract   : r) = (flip (-) <$> Pop <*> Pop) >>= Push >> evaluate r

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions = case runStateT (interpret $ evaluate instructions) [] of
  Just (x, s) -> Right x
  Nothing -> Left $ "RPN Instructions don't terminate, " ++ show instructions
