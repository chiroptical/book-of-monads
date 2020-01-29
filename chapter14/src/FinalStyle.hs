module FinalStyle where

-- Final Style

import Prelude hiding (writeFile)

class RandomGen m where
  random :: Int -> Int -> m Int

class FS m where
  writeFile :: FilePath -> String -> m (Either IOError ())
  readFile :: FilePath -> m (Either IOError String)

-- Composition is straight forward, need interpretations for `Monad m`
randomWrite :: (Monad m, FS m, RandomGen m) => FilePath -> m (Either IOError ())
randomWrite path = do
  number <- random 1 100
  writeFile path (show number)
