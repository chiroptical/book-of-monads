{-# LANGUAGE DataKinds #-}

module Main where

import Prelude hiding (writeFile)
import ExtensibleEffects

writeRandomNumberBetween :: FilePath -> Int -> Int -> Eff '[FS, RandomGen, Lift IO] (Either IOError ())
writeRandomNumberBetween fp start end = do
  randomNum <- random start end
  writeFile fp (show randomNum)

main :: IO (Either IOError ())
main = do
  runM . runFS $ writeFile "hello.txt" "hello"
  runM . runRandomGen . runFS $ writeRandomNumberBetween "randomNum.txt" 0 100
