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
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (createPool, withResource)
import Data.Time (NominalDiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Database.PostgreSQL.Entity.DBT.Types (ConnectionPool, DBError (..), QueryNature (..))
import Database.PostgreSQL.Simple as PG (ConnectInfo, FromRow, Query, ToRow, close, connect)
import qualified Database.PostgreSQL.Transact as PGT

-- | Execute a DBT action.
--
-- This function wraps the DBT actions in a 'try', so that the 'DBError' exceptions
-- raised will be converted to the Left branch of the Either.
--
-- @since 0.0.1.0
runDB :: (MonadBaseControl IO m)
      => ConnectionPool -> PGT.DBT m a -> m a
runDB pool action = withResource pool $ PGT.runDBTSerializable action

-- | Create a ConnectionPool with the appropriate parameters
--
-- @since 0.0.1.0
mkPool :: ConnectInfo     -- Database access information
       -> Int             -- Number of sub-pools
       -> NominalDiffTime -- Allowed timeout
       -> Int             -- Number of connections
       -> IO ConnectionPool
mkPool connectInfo subPools timeout connections =
  createPool (connect connectInfo) close subPools timeout connections

-- | Query wrapper that returns a 'Vector' of results
--
-- ⚠ This function may raise a 'DBError'
--
-- @since 0.0.1.0
query :: (ToRow params, FromRow result, MonadIO m)
          => QueryNature -> Query -> params -> PGT.DBT m (Vector result)
query queryNature q params = do
  logQueryFormat queryNature q params
  V.fromList <$> PGT.query q params

-- | Query wrapper that returns one result.
--
-- ⚠ This function may raise the following 'DBError':
--
-- * 'NotFound' if the query returns zero results
-- * 'TooManyResults' if the query returns more than one result
--
-- @since 0.0.1.0
queryOne :: (ToRow params, FromRow result, MonadIO m)
         => QueryNature -> Query -> params -> PGT.DBT m result
queryOne queryNature q params = do
  logQueryFormat queryNature q params
  result <- PGT.query q params
  pure $ listToOne result

-- | Query wrapper that returns a 'Vector' of results and does not take an argument
--
-- ⚠ This function may raise a 'DBError':
--
-- @since 0.0.1.0
query_ :: (FromRow result, MonadIO m)
       => QueryNature -> Query -> PGT.DBT m (Vector result)
query_ queryNature q = do
  logQueryFormat queryNature q ()
  V.fromList <$> PGT.query_ q

-- | Query wrapper for SQL statements which do not return.
--
-- @since 0.0.1.0
execute :: (ToRow params, MonadIO m)
        => QueryNature -> Query -> params -> PGT.DBT m Int64
execute queryNature q params = do
  logQueryFormat queryNature q params
  PGT.execute q params

listToOne :: [result] -> result
listToOne [r] = r
listToOne []  = throw NotFound
listToOne _   = throw TooManyResults

logQueryFormat :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> PGT.DBT m ()
logQueryFormat queryNature q params = do
  msg <- PGT.formatQuery q params
  case queryNature of
    Select -> liftIO $ cyanMessage   $ "[SELECT] " <> decodeUtf8 msg
    Update -> liftIO $ yellowMessage $ "[UPDATE] " <> decodeUtf8 msg
    Insert -> liftIO $ yellowMessage $ "[INSERT] " <> decodeUtf8 msg
    Delete -> liftIO $ redMessage    $ "[DELETE] " <> decodeUtf8 msg
