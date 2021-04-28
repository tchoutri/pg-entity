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
  ( ConnectionPool
  , QueryNature (..)
  ) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

-- | Type alias for the database pool
--
-- @since 0.0.1.0
type ConnectionPool = Pool Connection

-- | This sum type is given to the query, queryOne and execute functions to help
-- with logging.
--
-- @since 0.0.1.0
data QueryNature = Select | Insert | Update | Delete deriving (Eq, Show)
