{-|
  Module      : Database.PostgreSQL.Entity.DBT
  Copyright   : (c) Clément Delafargue, 2018
                    Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  The DBT plumbing module to handle database queries and pools
-}
module Database.PostgreSQL.Entity.DBT
  ( module Database.PostgreSQL.Entity.DBT.Types
  , execute
  , mkPool
  , queryMany
  , queryOne
  , runDB
  ) where

import Colourista.IO (cyanMessage, redMessage, yellowMessage)
import Control.Exception (throw)
import Control.Exception.Safe (MonadCatch, try)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Pool (createPool, withResource)
import qualified Database.PostgreSQL.Transact as PGT
import Database.PostgreSQL.Simple as PG (close, connect)
import Data.Time (NominalDiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Entity.DBT.Types

runDB :: (MonadCatch m, MonadBaseControl IO m)
      => ConnectionPool -> DBT m a -> m (Either DBError a)
runDB pool action = try $ withResource pool $ PGT.runDBTSerializable action

mkPool :: ConnectInfo -> Int -> NominalDiffTime -> Int -> IO ConnectionPool
mkPool connectInfo subPools timeout connections =
  createPool (connect connectInfo) close subPools timeout connections

queryMany :: (ToRow params, FromRow result, MonadIO m)
          => QueryNature -> Query -> params -> DBT m (Vector result)
queryMany queryNature q params = do
  logQueryFormat queryNature q params
  V.fromList <$> PGT.query q params

queryOne :: (ToRow params, FromRow result, MonadIO m)
         => QueryNature -> Query -> params -> DBT m result
queryOne queryNature q params = do
  logQueryFormat queryNature q params
  result <- PGT.query q params
  pure $ listToOne result

execute :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> DBT m ()
execute queryNature q params = do                                                 
  logQueryFormat queryNature q params                                             
  PGT.execute q params                                                            
  pure ()                                                                         

listToOne :: [result] -> result
listToOne [r] = r
listToOne []  = throw NotFound
listToOne _   = throw TooManyResults

logQueryFormat :: (ToRow params, MonadIO m) => QueryNature -> Query -> params -> DBT m ()
logQueryFormat queryNature q params = do                      
  msg <- PGT.formatQuery q params                      
  case queryNature of                      
    Select -> liftIO $ cyanMessage $ "[SELECT] " <> decodeUtf8 msg                      
    Update -> liftIO $ yellowMessage $ "[UPDATE] " <> decodeUtf8 msg                     
    Insert -> liftIO $ yellowMessage $ "[INSERT] " <> decodeUtf8 msg                     
    Delete -> liftIO $ redMessage $ "[DELETE] " <> decodeUtf8 msg
