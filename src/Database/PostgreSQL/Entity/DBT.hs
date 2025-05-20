{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  ( PGT.DBT
  , mkPool
  , withPool
  , execute
  , executeMany
  , query
  , query_
  , queryOne
  , queryOne_
  )
where

import Control.Monad.IO.Class
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, createPool, withResource)
import Data.Time (NominalDiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple as PG (ConnectInfo, Connection, FromRow, Query, ToRow, close, connect)
import qualified Database.PostgreSQL.Transact as PGT
import GHC.Stack

{-| Create a Pool Connection with the appropriate parameters

 @since 0.0.1.0
-}
mkPool
  :: HasCallStack
  => ConnectInfo -- Database access information
  -> Int -- Number of sub-pools
  -> NominalDiffTime -- Allowed timeout
  -> Int -- Number of connections
  -> IO (Pool Connection)
mkPool connectInfo subPools timeout connections =
  createPool (connect connectInfo) close subPools timeout connections

{-| Run a DBT action with no explicit error handling.

 This functions is suited for using 'MonadError' error handling.

 === __Example__

 > let e1 = E 1 True True
 > result <- runExceptT @EntityError $ do
 >   withPool pool $ insertEntity e1
 >   withPool pool $ markForProcessing 1
 > case result of
 >   Left err -> print err
 >   Right _  -> putStrLn "Everything went well"

 See the code in the @example/@ directory on GitHub

 @since 0.0.1.0
-}
withPool :: (HasCallStack, MonadIO m) => Pool Connection -> PGT.DBT IO a -> m a
withPool pool action = liftIO $ withResource pool (\conn -> PGT.runDBTSerializable action conn)

{-| Query wrapper that returns a 'Vector' of results

 @since 0.0.1.0
-}
query
  :: (HasCallStack, ToRow params, FromRow result, MonadIO m)
  => Query
  -> params
  -> PGT.DBT m (Vector result)
query q params = do
  V.fromList <$> PGT.query q params

{-| Query wrapper that returns a 'Vector' of results and does not take an argument

 @since 0.0.1.0
-}
query_
  :: (HasCallStack, FromRow result, MonadIO m)
  => Query
  -> PGT.DBT m (Vector result)
query_ q = do
  V.fromList <$> PGT.query_ q

{-| Query wrapper that returns one result.

 @since 0.0.1.0
-}
queryOne
  :: (HasCallStack, ToRow params, FromRow result, MonadIO m)
  => Query
  -> params
  -> PGT.DBT m (Maybe result)
queryOne q params = do
  listToMaybe <$> PGT.query q params

--

{-| Query wrapper that returns one result and does not take an argument

 @since 0.0.2.0
-}
queryOne_
  :: (HasCallStack, FromRow result, MonadIO m)
  => Query
  -> PGT.DBT m (Maybe result)
queryOne_ q = do
  listToMaybe <$> PGT.query_ q

{-| Query wrapper for SQL statements which do not return.

 @since 0.0.1.0
-}
execute
  :: (HasCallStack, ToRow params, MonadIO m)
  => Query
  -> params
  -> PGT.DBT m Int64
execute q params = do
  PGT.execute q params

{-| Query wrapper for SQL statements that operate on multiple rows which do not return.

 @since 0.0.2.0
-}
executeMany
  :: (HasCallStack, ToRow params, MonadIO m)
  => Query
  -> [params]
  -> PGT.DBT m Int64
executeMany q params = do
  PGT.executeMany q params
