{-# LANGUAGE StrictData #-}
{-|
  Module      : Database.PostgreSQL.Entity.DBT.Types
  Copyright   : (c) Clément Delafargue, 2018
                    Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Associated types provided to the library and users
-}
module Database.PostgreSQL.Entity.DBT.Types 
  ( DBError (..)
  , ConnectInfo
  , Connection
  , ConnectionPool
  , DBT
  , FromRow
  , Pool
  , Query
  , QueryNature (..)
  , ToRow
  ) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple as PG (ConnectInfo, FromRow, Query, ToRow)
import Database.PostgreSQL.Transact (DBT)

type ConnectionPool = Pool Connection

data QueryNature = Select | Insert | Update | Delete
  deriving (Show, Eq)

data DBError                          
  = ConstraintError {-# UNPACK #-} Text   
  | NotFound                                
  | TooManyResults                          
  | InsertionError                          
  | DeserialisationError {-# UNPACK #-} Text
  deriving stock (Show, Generic)            
                                            
instance Exception DBError
