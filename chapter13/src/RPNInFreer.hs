{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- Exercise 13.18 Rewrite IStack from `RPNInFree.hs` using freer style

module RPNInFreer where

import           Control.Monad                  ( (<=<) )
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )

data IStackI r where
  Pop ::IStackI Integer
  Push ::Integer -> IStackI ()

type IStack = Freer IStackI

pop :: IStack Integer
pop = Impure Pop return

push :: Integer -> IStack ()
push n = Impure (Push n) Pure

interpret' :: IStackI r -> StateT [Integer] Maybe r
interpret' Pop = do
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
interpret' (Push x) = modify ((:) x)

data RPNInstruction = Number Integer | Plus | Times | Subtract deriving Show

interpret :: IStack r -> StateT [Integer] Maybe r
interpret = foldFreer interpret'

evaluate :: [RPNInstruction] -> IStack Integer
evaluate []               = pop
evaluate ((Number n) : r) = push n >> evaluate r
evaluate (Plus       : r) = (+) <$> pop <*> pop >>= push >> evaluate r
evaluate (Times      : r) = (*) <$> pop <*> pop >>= push >> evaluate r
evaluate (Subtract   : r) = (-) <$> pop <*> pop >>= push >> evaluate r

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions = case runStateT (interpret $ evaluate instructions) [] of
  Just (x, s) -> Right x
  Nothing -> Left $ "RPN Instructions don't terminate, " ++ show instructions

data Freer instr a where
  Pure ::a -> Freer instr a
  Impure ::instr a -> (a -> Freer instr b) -> Freer instr b

instance Functor (Freer instr) where
  fmap f (Pure x    ) = Pure $ f x
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (Freer instr) where
  pure = Pure
  Pure f     <*> Pure x     = Pure $ f x
  f          <*> Impure x k = Impure x (\a -> f <*> k a)
  Impure x k <*> f          = Impure x (\a -> k a <*> f)

instance Monad (Freer instr) where
  Pure x     >>= f = f x
  Impure x k >>= f = Impure x (f <=< k)

foldFreer :: Monad m => (forall r . f r -> m r) -> Freer f a -> m a
foldFreer _         (Pure x    ) = return x
foldFreer interpret (Impure x f) = interpret x >>= foldFreer interpret . f
