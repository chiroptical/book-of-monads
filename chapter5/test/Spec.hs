{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import           Hedgehog
import           Hedgehog.Classes
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Control.Monad                  ( void
                                                , join
                                                )

-- Here lies the Hedgehog example in the documentation
-- prop_reverse :: Property
-- prop_reverse = property $ do
--   xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--   reverse (reverse xs) === xs

-- main :: IO ()
-- main =
--   void $ checkParallel $ Group "Test.Example" [("prop_reverse", prop_reverse)]

genInt :: Gen Int
genInt = Gen.int (Range.constant (-100) 100)

genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.constant 0 10)

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 100) genInt

genListFunction :: Gen ([Int] -> [[Int]])
genListFunction = do
  x <- genSmallInt
  Gen.choice $ pure <$> [ replicate x
                        , const $ replicate x []
                        ]

-- Need to implement genEitherFunction
genEitherStringInt :: Gen (Either String Int)
genEitherStringInt = Gen.choice
  [Right <$> genInt, Left <$> Gen.string (Range.constant 0 10) Gen.alpha]

genMaybeInt :: Gen (Maybe Int)
genMaybeInt = Gen.maybe genInt

genJustFunction :: Gen (Int -> Maybe Int)
genJustFunction =
  Gen.choice
    $   pure
    <$> [ Just . (+ 1)
        , Just . join (*) -- \x -> Just $ x * x
        , Just . subtract 10
        , Just . abs
        , const Nothing
        ]

instance Monad m => Show (a -> m a) where
  show _ = "a -> m a"

prop_justLeftIdentity :: Property
prop_justLeftIdentity = property $ do
  f <- forAll genJustFunction
  x <- forAll $ Gen.int (Range.constant 0 100)
  f x === (return x >>= f)

prop_justRightIdentity :: Property
prop_justRightIdentity = property $ do
  mx <- forAll genMaybeInt
  mx === (mx >>= return)

prop_justAssociativity :: Property
prop_justAssociativity = property $ do
  f  <- forAll genJustFunction
  g  <- forAll genJustFunction
  mx <- forAll genMaybeInt
  ((mx >>= f) >>= g) === (mx >>= (\x -> f x >>= g))

-- We can generalize the properties by taking in the function generation generator
prop_leftIdentity
  :: (Monad m, Show (m a), Show a, Eq (m a), Eq a)
  => Gen (a -> m a)
  -> Gen a
  -> Property
prop_leftIdentity genF genA = property $ do
  f <- forAll genF
  x <- forAll genA
  f x === (return x >>= f)

prop_rightIdentity :: (Monad m, Show (m a), Eq (m a)) => Gen (m a) -> Property
prop_rightIdentity genMA = property $ do
  mx <- forAll genMA
  mx === (mx >>= return)

prop_associativity
  :: (Monad m, Show (m a), Show a, Eq (m a), Eq a)
  => Gen (a -> m a)
  -> Gen (a -> m a)
  -> Gen (m a)
  -> Property
prop_associativity genF genG genMA = property $ do
  f  <- forAll genF
  g  <- forAll genG
  mx <- forAll genMA
  ((mx >>= f) >>= g) === (mx >>= (\x -> f x >>= g))

-- Idea from countoren: Use Monad Laws and check instances which fail
-- Idea from me: Use `monadLaws` built into Hedgehog.Classes to test Monads

main :: IO ()
main = do
  void $ checkParallel $ Group
    "Maybe Monad Laws"
    [ ("leftIdentity"       , prop_justLeftIdentity)
    , ("rightIdentity"      , prop_justRightIdentity)
    , ("associativity"      , prop_justAssociativity)
    ]
  void $ checkParallel $ Group
    "Maybe Monad Laws"
    [ ("leftIdentity"       , prop_leftIdentity genJustFunction genInt)
    , ("rightIdentity"      , prop_rightIdentity genMaybeInt)
    , ("associativity"      , prop_associativity genJustFunction genJustFunction genMaybeInt)
    ]
  void $ checkParallel $ Group
    "List Monad Laws"
    [ ("leftIdentity"       , prop_leftIdentity genListFunction genList)
    , ("rightIdentity"      , prop_rightIdentity genList)
    , ("associativity"      , prop_associativity genListFunction genListFunction (pure <$> genList))
    ]
