{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           System.Random                  ( randomRIO )
import           Control.Monad                  ( void
                                                , (>=>)
                                                )
import           Control.Exception              ( catch )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )
import qualified System.IO                     as SIO

-- 13.3.2 Streams as Initial Style Monads

-- Core `pipes` type

data Proxy a' a b' b m r
  = Request a' (a  -> Proxy a' a b' b m r)
  | Respond b  (b' -> Proxy a' a b' b m r)
  | M          (m    (Proxy a' a b' b m r))
  | Pure       r

-- Core `conduit` type

data Pipe l i o u m r
  = HaveOutput (Pipe l i o u m r)
  | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
  | Done r
  | PipeM (m (Pipe l i o u m r))
  | LeftOver (Pipe l i o u m r) l

-- Section 13.4 Operational Style and Freer Monads
