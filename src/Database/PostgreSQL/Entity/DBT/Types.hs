{-# LANGUAGE StrictData #-}
{-|
  Module      : Database.PostgreSQL.Entity.DBT.Types
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Associated types provided to the library and users
-}
module Database.PostgreSQL.Entity.DBT.Types
  ( DBError (..)
  , ConnectionPool
  , QueryNature (..)
  ) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

-- | Type alias for the database pool
type ConnectionPool = Pool Connection

-- | This sum type is given to the query, queryOne and execute functions to help
-- with logging.
data QueryNature = Select | Insert | Update | Delete deriving (Eq, Show)

-- | Database-facing errors. Unify them with your business-facing error data-type
-- when reporting.
data DBError
  = ConstraintError {-# UNPACK #-}Text
  | NotFound
  | TooManyResults
  | InsertionError
  | DeserialisationError {-# UNPACK #-}Text
  deriving stock (Generic, Show)

instance Exception DBError
