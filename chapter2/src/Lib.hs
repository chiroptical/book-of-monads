{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}

module Lib where

import           Control.Monad             (replicateM, replicateM_)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State

data Person =
  Person
    { personName :: String
    , personAge  :: Int
    }

validateName :: String -> Maybe String
validateName = undefined

validateAge :: Int -> Maybe Int
validateAge = undefined

validatePerson :: String -> Int -> Maybe Person
validatePerson name age =
  validateName name >>= \name' ->
    validateAge age >>= \age' -> return $ Person name' age'

validatePersonDo :: String -> Int -> Maybe Person
validatePersonDo name age = do
  name' <- validateName name
  age' <- validateAge age
  let isMinor = age' < 18
  pure $ Person name' age'

validatePersonApp :: String -> Int -> Maybe Person
validatePersonApp name age = Person <$> validateName name <*> validateAge age

validatePersonMonadComprehension :: String -> Int -> Maybe Person
validatePersonMonadComprehension name age =
  [Person n a | n <- validateName name, a <- validateAge age]

-- get :: Monad m => StateT s m s
-- get the internal state of the monad
-- x <- get -- x :: s
--
-- put :: Monad m => s -> StateT s m ()
-- update the internal state of the monad
-- put x, x :: s
-- incrCounter, gets the integer maintaining
-- the state, store the incremented integer,
-- return the result
incrCounter :: State Int Int
incrCounter = do
  n <- get
  put $ n + 1
  return $ n + 1

incrCounter' :: StateT Int IO Int
incrCounter' = do
  n <- get
  liftIO $ print n
  put $ n + 1
  get

incrCounterRealWorld :: State Int ()
incrCounterRealWorld = modify (+ 1)

incrCounterUnit :: State Int ()
incrCounterUnit = do
  n <- get
  put $ n + 1
  return ()

incrementN :: Int -> StateT Int IO ()
incrementN n = replicateM_ n incrCounter'

example :: IO ()
example = evalStateT (incrementN 2) 0

getNumber :: IO (Maybe Int)
getNumber = undefined

getNumber' :: IO Int
getNumber' = do
  n <- getNumber
  case n of
    Just m  -> pure m
    Nothing -> getNumber'

getNumber'' :: IO ()
getNumber'' =
  getNumber >>= \case
    Just n -> print $ "Your number is " ++ show n
    Nothing -> fail "Not a number :("
