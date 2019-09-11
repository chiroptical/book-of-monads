module Lib where

import           State
import Control.Monad.Trans.Reader
import Data.Functor.Identity

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
