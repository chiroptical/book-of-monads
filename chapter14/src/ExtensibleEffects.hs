{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module ExtensibleEffects where

import Freer

-- Section 14.3

data Union (rs :: [* -> *]) x where
  This :: f x -> Union (f : rs) x
  That :: Union rs x -> Union (f : rs) x

class Member f rs where
  inj :: f x -> Union rs x

instance Member f (f : rs) where
  inj = This

instance {-# overlappable #-} Member f rs => Member f (r : rs) where
  inj = That . inj

type Eff rs = Freer (Union rs)

data FS a where
  WriteFile :: FilePath -> String -> FS (Either IOError ())
  ReadFile :: FilePath -> FS (Either IOError String)

data RandomGen a where
  Random :: Int -> Int -> RandomGen Int

send :: Member f rs => f a -> Eff rs a
send x = Impure (inj x) Pure

writeFile :: Member FS rs => FilePath -> String -> Eff rs (Either IOError ())
writeFile = (send .) . WriteFile

readFile :: Member FS rs => FilePath -> Eff rs (Either IOError String)
readFile = send . ReadFile

random :: Member RandomGen rs => Int -> Int -> Eff rs Int
random = (send .) . Random
