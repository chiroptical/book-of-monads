{-# LANGUAGE RankNTypes #-}
module Lib where

import           Control.Monad.Managed
import           Control.Monad.Trans.Resource   ( runResourceT
                                                , allocate
                                                , release
                                                )
import           System.IO

--            Acquire    Release        Use
-- bracket :: IO r    -> (r -> IO b) -> (r -> IO a) -> IO a
-- bracket = undefined

-- bracket acquire release $ \r -> do ...

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
-- withFile fp mode = bracket (openFile fp mode) hClose

-- withFile inFile ReadMode $ \inHandle ->
--   withFile outFile WriteMode $ \outHandle ->
--     doSomething inHandle outHandle

doWork = undefined

process inFile outFile = runManaged $ do
  inHandle  <- managed (withFile inFile ReadMode)
  outHandle <- managed (withFile outFile WriteMode)
  liftIO $ doWork inHandle outHandle

-- Continuations
--
-- When talking about a type `a` we could express it
-- as a function `(a -> r) -> r`. The `(a -> r)`
-- _consumes_ `a` to generate `r`. Additionally,
-- `(a -> r) -> r` is representative of having
-- a function that can be applied to type `a`.

newtype Cont r a =
  Cont
    { runCont :: (a -> r) -> r
    }

toCont :: a -> (forall r. Cont r a)
toCont a = Cont $ \r -> r a

fromCont :: (forall r. Cont r a) -> a
fromCont (Cont f) = f id

-- "callback hell"
-- action1 $ \res1 ->
--   action2 $ \res2 ->
--     ..
--
-- do res1 <- action1
--    res2 <- action2

instance Functor (Cont r) where
  -- f :: a -> b
  -- arr :: (a -> r) -> r
  -- fmap ... :: (b -> r) -> r
  -- c :: b -> r
  fmap f (Cont arr) = Cont $ \c -> arr (c . f)

instance Applicative (Cont r) where
  pure a = Cont $ \c -> c a
  -- farr :: ((a -> b) -> r) -> r
  -- arr :: (a -> r) -> r
  -- (<*>) ... :: (b -> r) -> r
  -- c :: b -> r
  -- g :: a -> b
  (Cont farr) <*> (Cont arr) = Cont $ \c -> farr $ \g -> arr (c . g)

instance Monad (Cont r) where
  return = pure
  -- arr :: (a -> r) -> r
  -- f :: a -> Cont ((b -> r) -> r)
  -- (>>=) ... :: Cont ((b -> r) -> r)
  -- c :: b -> r
  Cont arr >>= f = Cont $ \c -> arr (\a -> runCont (f a) c)

-- Resource Pools

-- pool <- createPool
--   ( connectPostgresSql "host=localhost,port=1234" -- acquire
--     close -- release
--     1     -- number of sub-pools
--     0.5   -- seconds until an unused resource is collected
--     10    -- maximum number of connections
--   )
--
-- -- Somewhere else, get a resource and consume it
--
-- do
--   connection <- managed (withResource pool)
--   liftIO (query_ "select * from clients")
--   liftIO (query_ "select * from servers")

zipFiles :: FilePath -> FilePath -> FilePath -> IO ()
zipFiles fin1 fin2 fout =
  withFile fin1 ReadMode $ \in1 ->
    withFile fin2 ReadMode $ \in2 ->
      withFile fout WriteMode $ \out ->
        go in1 in2 out
  where
    go in1 in2 out = do
      eof1 <- hIsEOF in1
      if eof1 then end in2 out
              else do
                eof2 <- hIsEOF in2
                if eof2 then end in1 out
                        else do
                          hGetChar in1 >>= hPutChar out
                          hGetChar in2 >>= hPutChar out
                          go in1 in2 out
    end in_ out = do
      eof <- hIsEOF in_
      if eof then return ()
             else do
               hGetChar in_ >>= hPutChar out
               end in_ out

zipFilesWithRelease :: FilePath -> FilePath -> FilePath -> IO ()
zipFilesWithRelease fin1 fin2 fout = runResourceT $ do
  (rin1, in1) <- allocate (openFile fin1 ReadMode) hClose
  (rin2, in2) <- allocate (openFile fin2 ReadMode) hClose
  (rout, out) <- allocate (openFile fout WriteMode) hClose
  liftIO (go in1 rin1 in2 rin2 out rout)
    where
      go in1 rin1 in2 rin2 out rout = do
        eof1 <- liftIO $ hIsEOF in1
        if eof1 then release rin1 >> end in2 out rout
                else do
                  eof2 <- liftIO $ hIsEOF in2
                  if eof2 then release rin2 >> end in1 out rout
                          else do
                            liftIO $ hGetChar in1 >>= hPutChar out
                            liftIO $ hGetChar in2 >>= hPutChar out
                            go in1 rin1 in2 rin2 out rout
      end in_ out rout = do
        eof <- liftIO $ hIsEOF in_
        if eof then return ()
               else do
                 liftIO $ hGetChar in_ >>= hPutChar out
                 end in_ out rout


