module Main where

import           Control.Monad.Trans.State      ( execStateT )
import           Lib

main :: IO ()
main = do
  res <- execStateT foo []
  print res
