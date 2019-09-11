{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib

instance Show (State Int Int) where
  show _ = undefined

instance Arbitrary (State Int Int) where
  arbitrary = undefined

instance EqProp (State Int Int) where
  (=-=) = undefined

main :: IO ()
main = quickBatch $ monad (undefined :: State Int (Int, Int, Int))
