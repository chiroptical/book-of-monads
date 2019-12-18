{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module FinalInitialFreeExamples where

import           System.Random                  ( randomRIO )
import           Control.Monad                  ( void
                                                , (>=>)
                                                )
import           Control.Exception              ( catch )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )
import qualified System.IO                     as SIO

class FS m where
  writeFile :: FilePath -> String -> m (Either IOError ())
  readFile :: FilePath -> m (Either IOError String)

-- Cannot mix FS and IO actions freely
-- need to specialize m ~ IO for this
-- to work
--
-- f :: (Monad m, FS m) => FilePath -> m ()
-- f path = do
--   num <- randomRIO (1 :: Integer, 100)
--   void $ Lib.writeFile path (show num)

instance FS IO where
  writeFile path contents =
    (Right <$> Prelude.writeFile path contents) `catch` \ex -> return $ Left ex
  readFile path =
    (Right <$> Prelude.readFile path) `catch` \ex -> return $ Left ex

newtype MockFileSystem =
  MockFileSystem
    { getMockFileSystem :: Map FilePath String
    }

instance FS (State MockFileSystem) where
  writeFile path contents = Right
    <$> modify (MockFileSystem . Map.insert path contents . getMockFileSystem)

  readFile path = do
    mfs <- getMockFileSystem <$> get
    pure $ case Map.lookup path mfs of
      Nothing -> Left . userError $ "Path `" ++ path ++ "` does not exist!"
      Just contents -> Right contents

-- Initial Style, Section 13.3

-- These types just to make the code below compile
type Position = Int
type Player = String
type Result = Bool

info :: Position -> TicTacToe (Maybe Player)
take :: Position -> TicTacToe Result
info p = Info p return
take p = Take p return

data TicTacToe a =
    Info Position (Maybe Player -> TicTacToe a)
  | Take Position (Result -> TicTacToe a)
  | Done a

-- Exercise 13.5

from :: (() -> a) -> a
from f = f ()
-- from = ($ ()) -- for point-free implementation

to :: a -> (() -> a)
to = const

-- Implementing the Monad Instance for `TicTacToe`

instance Monad TicTacToe where
  Done x   >>= f = f x
  Info p g >>= f = Info p (g >=> f)
  Take p g >>= f = Take p (g >=> f)

-- Exercise 13.6

instance Functor TicTacToe where
  fmap f (Done x    ) = Done (f x)
  fmap f (Info pos g) = Info pos (fmap f . g)
  fmap f (Take pos g) = Take pos (fmap f . g)

instance Applicative TicTacToe where
  pure = Done
  Done f     <*> Done x      = Done $ f x
  Info pos f <*> Info pos' x = Info
    (pos + pos')
    (\y ->
      let f' = f y
          x' = x y
      in  f' <*> x'
    )
  Take pos f <*> Take pos' x = Take
    (pos + pos')
    (\y ->
      let f' = f y
          x' = x y
      in  f' <*> x'
    )

takeIfNotTaken :: Position -> TicTacToe (Maybe Result)
takeIfNotTaken p = do
  i <- info p
  case i of
    Just _  -> return Nothing
    Nothing -> Just <$> Lib.take p

-- Most common interpretations take the form
-- i.e. Natural transformations
-- interpret :: TicTacToe a -> OtherMonad a

-- Very similar to the TicTacToe example in final style
-- runGame :: TicTacToe a -> ReaderT Player (StateT Board IO) a
-- runGame (Done x) = return x
-- runGame (Info p k) = do pl <- lookup p <$> get
--                         runGame (k pl)
-- runGame (Take p k) = do pl <- lookup p <$> get
--                         case pl of
--                           Just p' -> runGame (k $ AlreadyTaken { by = p' })
--                           Nothing -> do me <- ask
--                                         modify (insert p me)
--                                         ...

data FFS a =
    FWriteFile FilePath String (Either IOError () -> FFS a)
  | FReadFile FilePath         (Either IOError String -> FFS a)
  | FFSDone                    a

fWriteFile :: FilePath -> String -> FFS (Either IOError ())
fWriteFile path contents = FWriteFile path contents FFSDone

fReadFile :: FilePath -> FFS (Either IOError String)
fReadFile path = FReadFile path FFSDone

-- Exercise 13.7 Write a computation that mixes file input via
-- the FFS Monad with some operation in IO. What kind of
-- error do you expect to raise?
--
-- Similar error to first final style implementation

interpret :: FFS a -> IO a
interpret (FFSDone x) = return x
interpret (FWriteFile path contents k) =
  (SIO.writeFile path contents >> interpret (k $ Right ()))
    `catch` \ex -> interpret (k $ Left ex)
interpret (FReadFile path k) =
  do
      contents <- SIO.readFile path
      interpret (k (Right contents))
    `catch` \ex -> interpret (k (Left ex))

-- Exercise 13.8, write interpret for FFS w/ the State Monad

interpret' :: FFS a -> State MockFileSystem a
interpret' (FFSDone x                 ) = return x
interpret' (FWriteFile path contents k) = do
  mfs <- get
  modify (MockFileSystem . Map.insert path contents . getMockFileSystem)
  interpret' (k $ Right ())
interpret' (FReadFile path k) = do
  mfs <- get
  case Map.lookup path (getMockFileSystem mfs) of
    Just contents -> interpret' (k $ Right contents)
    Nothing       -> interpret'
      (k . Left . userError $ "Path `" ++ path ++ "` does not exist!")

-- 13.3.1 Free Monads

data TicTacToeF r =
    InfoF Position (Maybe Player -> r)
  | TakeF Position (Result -> r)

-- Exercise 13.9

instance Functor TicTacToeF where
  fmap f (InfoF p g) = InfoF p (f . g)
  fmap f (TakeF p g) = TakeF p (f . g)

-- Lost recursive nature of initial style
-- Free data type gets that back

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free g) = Free $ fmap (f <$>) g

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x@(Pure _) = f <$> x
  Pure f <*> x@(Free _) = f <$> x
  -- f :: Free f (a -> b)
  -- <*> x :: Free f (a -> b) -> Free f b
  Free f <*> x          = Free $ fmap (<*> x) f

instance Functor f => Monad (Free f) where
  Pure x >>= f = f x
  Free x >>= f = Free $ fmap (>>= f) x

type TicTacToe' = Free TicTacToeF

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

info' :: Position -> Free TicTacToeF (Maybe Player)
info' p = liftF (InfoF p id)

-- Exercise 13.11 Rewrite FS as a Free Monad

data FSF r =
    WriteFile' FilePath String (Either IOError () -> r)
  | ReadFile'  FilePath        (Either IOError String -> r)

instance Functor FSF where
  fmap f (WriteFile' path contents g) = WriteFile' path contents (f . g)
  fmap f (ReadFile' path g          ) = ReadFile' path (f . g)

type FS' = Free FSF

interpretFSF' :: FSF a -> IO a
interpretFSF' (WriteFile' path contents k) =
  (SIO.writeFile path contents >> (pure . k $ Right ()))
    `catch` \ex -> pure . k $ Left ex
interpretFSF' (ReadFile' path k) =
  (SIO.readFile path >>= (pure . k . Right)) `catch` (pure . k . Left)

foldFree :: Monad m => (forall r . f r -> m r) -> Free f a -> m a
foldFree _         (Pure x) = return x
foldFree interpret (Free x) = do
  x' <- interpret x
  foldFree interpret x'

interpretFSF :: FS' a -> IO a
interpretFSF = foldFree interpretFSF'
