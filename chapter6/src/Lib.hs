module Lib where

import           State
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Writer

import Data.Functor.Contravariant

-- This is a bad idea!
-- modify internal state similar to accessing
-- private methods or members from classes
nextValue :: State Int Int
nextValue = State $ \i -> (i, i + 1)

-- Incrementally beta reduce, to (0, 0)
-- lib :: (Int, Int)
-- lib = runState
--   ( do
--       a <- get
--       return a
--   )
--   0
-- 2. Replace do with (>>=)
-- 3. Replace replace get
-- lib :: (Int, Int)
-- lib = runState
--   (State (\s -> (s, s)) >>= \a -> return a)
--   0
-- 4. Replace definition of (>>=)
-- lib :: (Int, Int)
-- lib = runState
--   ( State (\s' -> let (a, s'') = (\s -> (s, s)) s'
--                    in runState ((\x -> return x) a) s'')
--   )
--   0
-- 5. Apply runState
-- lib :: (Int, Int)
-- lib =
--   ((\s' -> let (a, s'') = (\s -> (s, s)) s'
--             in runState ((\x -> return x) a) s'')
--   )
--   0
-- 6. Apply function
-- lib :: (Int, Int)
-- lib =
--   let (a, s'') = (\s -> (s, s)) 0
--    in runState ((\x -> return x) a) s''
-- 7. Apply function
-- lib :: (Int, Int)
-- lib =
--   let (a, s'') = (0, 0)
--    in runState ((\x -> return x) a) s''
-- 8. Replace a and s';
-- lib :: (Int, Int)
-- lib =
--    runState ((\x -> return x) 0) 0
-- 9. Apply lambda
-- lib :: (Int, Int)
-- lib =
--    runState (State $ \x -> (0, x)) 0
-- 10. runState
-- lib :: (Int, Int)
-- lib =
--    (\x -> (0, x)) 0
-- 11. Apply lambda
-- lib :: (Int, Int)
-- lib = (0, 0)

-- Section 6.5
mapWriter :: (v -> w) -> Writer v a -> Writer w a
mapWriter f (Writer (v, a)) = Writer (f v, a)

class Bifunctor f where
  first :: (v -> w) -> f v a -> f w a
  second :: (a -> b) -> f v a -> f v b
  bimap :: (v -> w) -> (a -> b) -> f v a -> f w b

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader = undefined

-- Exercise 6.3, consider the following type, implement Contravariant

newtype Return r a = Return (a -> r)

instance Contravariant (Return r) where
  -- contramap :: (a -> b) -> f b -> f a
  contramap f (Return ra) = Return $ ra . f
