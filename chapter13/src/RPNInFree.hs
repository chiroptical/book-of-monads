{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module RPNInFree where

import FinalInitialFreeExamples (Free (..), liftF, foldFree)
import Control.Monad.Trans.State
import           Control.Monad                  ( guard )

data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g x) = Coyoneda (f . g) x

type Freer f = Free (Coyoneda f)

-- 13.5 Transforming and Inspecting Computations

data IStackF r = Pop (Integer -> r) | Push Integer r deriving Functor
type IStack = Free IStackF

pop :: IStack Integer
pop = liftF (Pop id)

push :: Integer -> IStack ()
push v = liftF (Push v ())

-- Reverse Polish Notation Calculator
-- 3 2 + 1 4 + x ~ (3 + 2) x (1 + 4)

data RPNInstruction = Number Integer | Plus | Times | Subtract

evaluate :: [RPNInstruction] -> IStack Integer
evaluate [] = pop
evaluate ((Number n) : r) = push n >> evaluate r
evaluate (Plus : r) = ((+) <$> pop <*> pop) >>= push >> evaluate r
evaluate (Times : r) = ((*) <$> pop <*> pop) >>= push >> evaluate r
evaluate (Subtract : r) = ((-) <$> pop <*> pop) >>= push >> evaluate r

-- Exercise 13.17: Write an interpreter from IStack to State [Integer]. Use that interpreter
-- to write a function from [RPNInstruction] -> Integer

interpret' :: IStackF r -> State [Integer] r
interpret' (Push x r) = do
  modify (++[x])
  pure r
interpret' (Pop ir) = do
  xs <- get
  let (s, x) = pop xs
  put s
  pure $ ir x
    where
      pop :: [a] -> ([a], a)
      pop [] = error "Two values must be on the stack to call operations"
      pop xs = (init xs, last xs)

interpret :: IStack r -> State [Integer] r
interpret = foldFree interpret'

rpn :: [RPNInstruction] -> Either String Integer
rpn instructions = 
  let (x, s) = runState (interpret $ evaluate instructions) []
   in if null s then Right x
                else Left $ "RPN Instructions don't terminate, stack: " ++ show s ++ " and value: " ++ show x

optimize :: IStack a -> IStack a
optimize x@(Pure _) = x
optimize (Free (Push v (Free (Pop k)))) = optimize (k v)
optimize (Free (Pop k)) = Free (Pop (optimize . k))
optimize (Free (Push v k)) = Free (Push v (optimize k))
