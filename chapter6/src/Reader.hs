module Reader where

-- Terminology below is a reminder to self
newtype Reader r a =
--         ^ is called a "type constructor"
  Reader
--  ^ is called a "data constructor"
    { runReader :: r -> a
--       ^ is called a "field"
    }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rab <*> Reader ra = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
  return = pure
  Reader ra >>= farrb = Reader $ \r -> runReader (farrb . ra $ r) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader rs (Reader sa) = Reader $ sa . rs

-- handle :: Config -> Request -> Response
-- handle cfg req = produceResponse cfg (initializeHeader cfg) (getArguments cfg req)
-- Refactor the above with the Reader Monad
-- Think of `cfg` as being threaded through the computation
-- handle :: Request -> Reader Config Response
-- handle req = do
--   header <- initializeHeader
--   args <- getArguments req
--   produceResponse header args

-- Use withReader to operate on a subset
-- of fields, example
-- data Config =
--   Config
--     { userConfig :: UserConfig
--     , dbConfig :: DbConfig
--     , logConfig :: LogConfig
--     }

-- handle = do
--   q <- ...
--   r <- withReader dbConfig (query q)
--   ...
--   where
--     query :: DatabaseQuery -> Reader DbConfig Result
--     query q = do
--       dbc <- ask
--       ...
