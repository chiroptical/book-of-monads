{-# LANGUAGE ApplicativeDo #-}

module ApplicativeDoExample where

example :: Applicative f => (a -> b) -> f a -> f b
example f fa = do
  a <- fa
  return $ f a
