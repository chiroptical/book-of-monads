{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           Data.STRef
import           Control.Monad.ST
import           Control.Exception
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad

-- data AlgState =
--   AlgState
--     { counter :: Int
--     , visited :: [Node]
--     }

-- travel :: [Note] -> State AlgState ()
-- travel (n:ns) = do ...
--   -- query and update counter
--   c <- gets counter
--   modify (\s -> s { counter = c + 1 })

--   ...

--   -- push an element into the list of nodes
--   modify (\s -> s { visited = n : visited s })

--   ...

-- notAllowed = let var = runST (newSTRef 0)
--               in runST (readSTRef var)

-- 8.2 Interfacing with the "Real" World

greet :: IO ()
greet = do
  putStr "Enter your name: "
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"

a :: IO Int
a = pure 1

b :: IO Int
b = pure 1

c = (==) <$> a <*> b

d :: [IO ()]
d = map putStrLn ["Barry", "Jappie"]

f = mapM_ putStrLn ["Barry", "Jappie"]

e = sequence_ d

-- 8.2.1 Exceptions

data DatabaseException = ConnectionError
  deriving Show
instance Exception DatabaseException

g amount numberOfPeople = print (amount / numberOfPeople)
  `catch` \(e :: ArithException) -> putStrLn "wrong number of people"

h a n = handle (\(e :: SomeException) -> putStrLn "broke") (print (a / n))

i amount numberOfPeople =
  print (amount / numberOfPeople)
    `catches` [ Handler
                $ \(e :: ArithException) -> putStrLn "wrong number of people"
              , Handler $ \(e :: SomeException) -> putStrLn "what happened?"
              ]

ae :: Rational -> Rational -> IO Rational
ae n d = evaluate (n / d) `catch` \(e :: ArithException) -> do
  putStrLn $ "can't divide " ++ show n ++ " with " ++ show d
  return 0

-- 8.2.2 Impure References

-- 8.2.3 Transactional References

addName :: TVar Integer -> TVar [(Integer, String)] -> String -> STM ()
addName counter names name = do
  i  <- readTVar counter
  ns <- readTVar names
  -- 1. Simple if implementation
  if any ((== name) . snd) ns
    then return ()
    else writeTVar names ((i, name) : ns) >> writeTVar counter (i + 1)
  -- ---
  -- 2 and 3 suffer from a issue with retry, only the above
  -- exhibits the behavior we are looking for.
  -- Also, 1 is much more readable
  -- ---
  -- 2. Writen w/ orElse and guard
  -- orElse (guard $ any ((== name) . snd) ns)
  --   (writeTVar names ((i, name) : ns) >> writeTVar counter (i + 1))
  -- 3. Lumie's solution (similar to two, but flipped guard)
  -- let addName' = do
  --       guard $ all ((/= name) . snd) ns
  --       writeTVar names ((i, name) : ns)
  --       writeTVar counter (i + 1)
  -- orElse addName' $ return ()

n :: STM (TVar [(Integer, String)])
n = newTVar [(0, "Barry")]

m :: STM (TVar Integer)
m = newTVar 0

exAddName :: String -> IO ()
exAddName name = do
  n' <- atomically n
  m' <- atomically m
  atomically (addName m' n' name)
  readTVarIO n' >>= print
