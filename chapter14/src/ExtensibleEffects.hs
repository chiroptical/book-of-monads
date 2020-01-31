{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module ExtensibleEffects where

import System.IO (readFile, writeFile)

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

data Reader r x where
  Ask :: Reader r r

proj :: Union (f : rs) x -> Either (Union rs x) (f x)
proj (This t) = Right t
proj (That t) = Left t

runReader :: r -> Eff (Reader r : rs) a -> Eff rs a
runReader r = loop
  where
    loop (Pure x) = return x
    loop (Impure a k) = case proj a of
                          Right Ask -> loop (k r)
                          Left op -> Impure op (loop . k)

run :: Eff '[] a -> a
run (Pure x) = x

-- freer-simple
-- runState :: s -> Eff (State s : rs) a -> Eff rs (a, s)
-- runError :: Eff (Error e : rs) a -> Eff rs (Either e a)
-- makeChoiceA :: Alternative f => Eff (NonDet : rs) a -> Eff rs (f a)

data State s x where
  Get :: State s s
  Put :: s -> State s ()

runReaderS :: Eff (Reader r : rs) a -> Eff (State r : rs) a
runReaderS = loop
  where
    loop :: Eff (Reader r : rs) a -> Eff (State r : rs) a
    loop (Pure x) = return x
    loop (Impure a k) = case proj a of
                          Right Ask -> Impure (inj Get) (loop . k)
                          Left op -> Impure (That op) (loop . k)

data Error e x where
  Error :: e -> Error e a

handleError :: Eff (Error e : rs) a -> (e -> Eff rs a) -> Eff rs a
handleError m c = loop m
  where
    loop (Pure x) = return x
    loop (Impure a k) = case proj a of
                          Right (Error e) -> c e
                          Left op -> Impure op (loop . k)

-- catchError :: Member (Error e) rs => Eff rs a -> (e -> Eff rs a) -> Eff rs a

-- runFS :: Eff (FS : rs) a -> IO (Eff rs a)
-- runFS = _a

newtype Lift m a = Lift (m a)

runFS :: Member (Lift IO) rs => Eff (FS : rs) a -> Eff rs a
runFS = loop
  where
    loop :: Member (Lift IO) rs => Eff (FS : rs) a -> Eff rs a
    loop (Pure x) = return x
    loop (Impure a k) = case proj a of
                          Right (ReadFile fp) -> loop . k $ _a
                          Right (WriteFile fp contents) -> _b
                          Left op -> Impure op (loop . k)

runM :: Monad m => Eff (Lift m : '[]) a -> m a
runM = loop
  where
    loop (Pure x) = return x
    loop (Impure a k) = case proj a of
                          Right (Lift a') -> a' >>= loop . k
                          Left _ -> error "Not possible..."
