{-|
  Module      : Database.PostgreSQL.Entity.Types.Unsafe
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
                  Koz Ross, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : Experimental

  Contains the internals of several key types.

  = Note

  By using these directly, you run the risk of violating internal invariants,
  or making representational changes in incompatible ways. This API is not
  stable, and is not subject to the PVP. Use at your own risk

  If at all possible, instead use the API provided by
  'Database.PostgreSQL.Entity.Types'.

-}
module Database.PostgreSQL.Entity.Types.Unsafe
  (
    Field (..)
  ) where

import Data.Text (Text)

-- | A wrapper for table fields.
--
-- @since 0.0.1.0
data Field
  = Field Text (Maybe Text)
  deriving stock (Eq, Show)

