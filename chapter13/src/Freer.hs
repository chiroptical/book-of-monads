{-# LANGUAGE GADTs #-}

module Freer where

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad                  ( (<=<)
                                                , (>=>)
                                                )

-- 13.3.2 Streams as Initial Style Monads

-- Core `pipes` type

data Proxy a' a b' b m r
  = Request a' (a  -> Proxy a' a b' b m r)
  | Respond b  (b' -> Proxy a' a b' b m r)
  | M          (m    (Proxy a' a b' b m r))
  | Pure       r

-- Core `conduit` type

data Pipe l i o u m r
  = HaveOutput (Pipe l i o u m r)
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
  | Done' r
  | PipeM (m (Pipe l i o u m r))
  | LeftOver (Pipe l i o u m r) l

-- Sections 13.4, past Exercise 13.3

-- initial style -> free monad
-- operational style -> freer monad

type Position = Int
type Player = String
type Result = Bool
type Board = Map Position Player

-- I for _instructions_
data TicTacToeI a where
  Info ::Position -> TicTacToeI (Maybe Player)
  Take ::Position -> TicTacToeI Result

data Program instr a where
  Done ::a -> Program instr a
  Bind ::Program instr a -> (a -> Program instr b) -> Program instr b
  Instr ::instr a -> Program instr a

instance Functor (Program instr) where
  fmap f x = undefined -- same as before, ignoring

instance Applicative (Program instr) where
  pure = Done
  f <*> x = undefined -- same as before, ignoring

instance Monad (Program instr) where
  (>>=) = Bind

-- The Program datatype allows us to write the instructions and
-- get a _freer_ Monad

-- 13.4.1 Operational Monads as Free Monads

-- Exercise 13.15, implement Functor and Applicative

data Freer instr a where
  Pure' ::a -> Freer instr a
  Impure ::instr a -> (a -> Freer instr b) -> Freer instr b

instance Functor (Freer instr) where
  fmap f (Pure' x   ) = Pure' $ f x
  fmap f (Impure x k) = Impure x (fmap f . k)

instance Applicative (Freer instr) where
  pure = Pure'
  Pure' f    <*> Pure' x    = Pure' $ f x
  Impure x k <*> f          = Impure x (\a -> k a <*> f)
  Pure' f <*> Impure x k    = Impure x (fmap f . k)

instance Monad (Freer instr) where
  Pure' x    >>= f = f x
  Impure x k >>= f = Impure x (f <=< k)

-- Exercise 13.16: Write functions `twoToThree` and `threeToTwo` that convert
-- between the freer monads with two and three constructors.

twoToThree :: Freer instr a -> Program instr a
twoToThree (Pure' x   ) = Done x
twoToThree (Impure x f) = Bind (Instr x) $ twoToThree . f

threeToTwo :: Program instr a -> Freer instr a
threeToTwo (Done x             ) = Pure' x
threeToTwo (Instr ia           ) = Impure ia Pure'
threeToTwo (Bind (Done x) f) = threeToTwo $ f x
threeToTwo (Bind (Instr ia) f) = Impure ia (threeToTwo . f)

-- f' :: b -> Program instr c
-- f  :: a -> Program instr b
-- x  :: Program instr a
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
threeToTwo (Bind (Bind x f') f) = threeToTwo $ x >>= (f' >=> f)
