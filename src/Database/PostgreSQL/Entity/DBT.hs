{-|
  Module      : Database.PostgreSQL.Entity.DBT
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  The 'Database.PostgreSQL.Transact.DBT' plumbing module to handle database queries and pools
-}
module Database.PostgreSQL.Entity.DBT
  ( mkPool
  , runDB
  , execute
  , query
  , query_
  , queryOne
  , module Database.PostgreSQL.Entity.DBT.Types
  ) where

import Colourista.IO (cyanMessage, redMessage, yellowMessage)
import Control.Exception (throw)
import Control.Exception.Safe (MonadCatch, try)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (createPool, withResource)
import Data.Time (NominalDiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Database.PostgreSQL.Entity.DBT.Types
import Database.PostgreSQL.Simple as PG (ConnectInfo, FromRow, Query, ToRow, close, connect)
import qualified Database.PostgreSQL.Transact as PGT

-- | Execute a DBT action.
--
-- This function wraps the DBT actions in a 'try', so that the exceptions raised
-- will be converted to the Left branch of the Either.
runDB :: (MonadCatch m, MonadBaseControl IO m)
      => ConnectionPool -> PGT.DBT m a -> m (Either DBError a)
runDB pool action = try $ withResource pool $ PGT.runDBTSerializable action

-- | Create a ConnectionPool with the appropriate parameters
mkPool :: ConnectInfo     -- Database access information
       -> Int             -- Number of sub-pools
       -> NominalDiffTime -- Allowed timeout
       -> Int             -- Number of connections
       -> IO ConnectionPool
mkPool connectInfo subPools timeout connections =
  createPool (connect connectInfo) close subPools timeout connections

-- | Query building block that returns a 'Vector' of results
query :: (ToRow params, FromRow result, MonadIO m)
          => QueryNature -> Query -> params -> PGT.DBT m (Vector result)
query queryNature q params = do
  logQueryFormat queryNature q params
  V.fromList <$> PGT.query q params

-- | Query building block that returns one result.
--
-- ⚠ This function will raise the following 'DBError':
--
-- * 'NotFound' if the query returns zero results
-- * 'TooManyResults' if the query returns more than one result
queryOne :: (ToRow params, FromRow result, MonadIO m)
         => QueryNature -> Query -> params -> PGT.DBT m result
queryOne queryNature q params = do
  logQueryFormat queryNature q params
  result <- PGT.query q params
  pure $ listToOne result

query_ :: (FromRow result, MonadIO m)
       => QueryNature -> Query -> PGT.DBT m (Vector result)
query_ queryNature q = do
  logQueryFormat queryNature q ()
  V.fromList <$> PGT.query_ q

-- | Query building block for SQL statements which do not return.
execute :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> PGT.DBT m ()
execute queryNature q params = do
  logQueryFormat queryNature q params
  PGT.execute q params
  pure ()

listToOne :: [result] -> result
listToOne [r] = r
listToOne []  = throw NotFound
listToOne _   = throw TooManyResults

logQueryFormat :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> PGT.DBT m ()
logQueryFormat queryNature q params = do
  msg <- PGT.formatQuery q params
  case queryNature of
    Select -> liftIO $ cyanMessage $ "[SELECT] " <> decodeUtf8 msg
    Update -> liftIO $ yellowMessage $ "[UPDATE] " <> decodeUtf8 msg
    Insert -> liftIO $ yellowMessage $ "[INSERT] " <> decodeUtf8 msg
    Delete -> liftIO $ redMessage $ "[DELETE] " <> decodeUtf8 msg
