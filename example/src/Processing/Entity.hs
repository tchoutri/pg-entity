module Processing.Entity where

import Control.Monad.Except
import Database.PostgreSQL.Transact
import GHC.Generics
import Data.Pool

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Entity.DBT

data E = E
  { key :: Int
  , processing :: Bool
  , state :: Bool
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromRow, ToRow)
  deriving Entity
    via (GenericEntity '[TableName "entities"] E)

data EntityError
  = EntityNotFound
  | EntityBadState
  | EntityProcessingIsRunning
  deriving (Eq, Show)

insertEntity :: (MonadIO m)
             => E -> DBT m ()
insertEntity = insert @E

getEntity :: (MonadError EntityError m, MonadIO m)
          => Int -> DBT m E
getEntity key = do
  result <- selectById (Only key)
  case result of
    Just e -> pure e
    Nothing -> lift $ throwError EntityNotFound

-- | We fetch the entity by its key
-- We check if it is in a good state by checking the flag
--   If the 'state' flag is False, we throw EntityBadState
-- We check if it is already processing by checking the flag
--  If the 'processing' flag is True, we throw EntityProcessingIsRunning
-- Otherwise, we switch the the 'processing' flag to true
markForProcessing :: (MonadError EntityError m, MonadIO m)
              => Int -> DBT m ()
markForProcessing key = do
  entity <- getEntity key
  checkSanity entity
  let newEntity = entity{processing = True}
  update @E newEntity
  where
    checkSanity entity | not (state entity) = lift $ throwError EntityBadState
                       | processing entity = lift $ throwError EntityProcessingIsRunning
                       | otherwise = pure ()

makePool :: IO (Pool Connection)
makePool = mkPool connectInfo 10 2 10
  where
    connectInfo = defaultConnectInfo{connectPassword="postgres"}
