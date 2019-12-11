{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           System.Random                  ( randomRIO )
import           Control.Monad                  ( void )
import           Control.Exception              ( catch )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )

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
  writeFile path contents =
    Right <$> modify (MockFileSystem . Map.insert path contents . getMockFileSystem)

  readFile path = do
    mfs <- getMockFileSystem <$> get
    pure $ case Map.lookup path mfs of 
      Nothing -> Left . userError $ "Path `" ++ path ++ "` does not exist!"
      Just contents -> Right contents
