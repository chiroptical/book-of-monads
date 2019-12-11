{-# LANGUAGE FlexibleInstances #-}
module TicTacToe where

import Prelude hiding (take)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.IO.Class         ( liftIO )

--           Bottom | Middle | Top
--           Left   | Middle | Right
data Three = One | Two | Three
  deriving (Eq, Show, Ord)

data Position = 
  Position
    { row :: Three
    , column :: Three
    } deriving (Eq, Ord, Show)

toPosition :: Three -> Three -> Position
toPosition = Position

data Player = X | O deriving (Eq, Show)

data Result = AlreadyTaken { by :: Player }
            | NextTurn
            | GameEnded { winner :: Player }

class TicTacToe m where
  info :: Three -> Three -> m (Maybe Player)
  take :: Three -> Three -> m Result

emptyBoard :: (Monad m, TicTacToe m) => Three -> Three -> m (Maybe Player)
emptyBoard = const . const . return $ Nothing

takeIfNotTaken :: (Monad m, TicTacToe m) => Three -> Three -> m (Maybe Result)
takeIfNotTaken row col = do
  i <- info row col
  case i of
    Just _ -> return Nothing
    Nothing -> Just <$> take row col

type Board = Map Position Player

instance TicTacToe (ReaderT Player (StateT Board IO)) where
  info row col = M.lookup (toPosition row col) <$> lift get
  take row col = do
    l <- info row col
    case l of
      Just p -> return AlreadyTaken { by = p }
      Nothing -> do
        me <- ask
        lift $ modify (M.insert (toPosition row col) me)
        -- if winner
        -- then pure $ GameEnded { winner = me }
        -- else do
        liftIO $ putStrLn "Your next move:"
        pure NextTurn

logic :: (Monad m, TicTacToe m) => m Result
logic = undefined

runTicTacToe :: IO (Result, Board)
runTicTacToe = runStateT (runReaderT logic X) M.empty
