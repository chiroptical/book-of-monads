{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module OperationalStyle where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Functor.Identity
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader

-- Section 13.4 Operational Style and Freer Monads

type Position = Int
type Player = String
type Result = Bool
type Board = Map Position Player

data TicTacToe a where
  Info :: Position -> TicTacToe (Maybe Player)
  Take :: Position -> TicTacToe Result
  Done' :: a -> TicTacToe a
  Bind :: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b

-- 13.12 Write the Functor instance...
instance Functor TicTacToe where
  -- fmap f a = a >>= pure . f
  -- fmap f = flip >>= pure . f
  fmap = (=<<) . (pure .)

instance Applicative TicTacToe where
  pure  = Done'
  -- f <*> x = (f >>=) . (`fmap` x)
  (<*>) = (. flip fmap) . (>>=)

instance Monad TicTacToe where
  (>>=) = Bind

info :: Position -> TicTacToe (Maybe Player)
take :: Position -> TicTacToe Result
info = Info
take = Take

takeIfNotTake p = do
  i <- info p
  case i of
    Just _  -> return Nothing
    Nothing -> Just <$> OperationalStyle.take p

-- runGame :: TicTacToe a -> ReaderT Player (StateT Board IO) a
-- runGame (Done' x ) = pure x
-- runGame (Bind x f) = runGame x >>= runGame . f
-- runGame (Info p) = Map.lookup p <$> get
-- runGame (Take p) = do
--   pl <- Map.lookup p <$> get
--   case pl of
--     Just p' -> return $ AlreadyTaken { by = p' }
--     Nothing -> do
--       me <- ask
--       modify (Map.insert p me)
--       ...

-- 13.13 Rewrite the FS Monad in operational style
newtype MockFileSystem =
  MockFileSystem
    { getMockFileSystem :: Map FilePath String
    }

data FS a where
  WriteFile :: FilePath -> String -> FS (Either IOError ())
  ReadFile :: FilePath -> FS (Either IOError String)
  FsDone :: a -> FS a
  FsBind :: FS a -> (a -> FS b) -> FS b

instance Functor FS where
  fmap = (=<<) . (pure .)

instance Applicative FS where
  pure = FsDone
  (<*>) = (. flip fmap) . (>>=)

instance Monad FS where
  (>>=) = FsBind

runFS :: FS a -> State MockFileSystem a
runFS (FsDone x) = pure x
runFS (FsBind x f) = runFS x >>= runFS . f
runFS (WriteFile path contents) = Right <$> modify (MockFileSystem . Map.insert path contents . getMockFileSystem)
runFS (ReadFile path) = do
  mockFs <- get
  pure $ case Map.lookup path (getMockFileSystem mockFs) of
    Just result -> Right result
    Nothing -> Left . userError $ "Path `" ++ path ++ "` does not exist!"
