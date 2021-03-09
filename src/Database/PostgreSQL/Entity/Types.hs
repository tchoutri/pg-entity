{-|
  Module      : Database.PostgreSQL.Entity.Types
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Typeclasses and types

-}
module Database.PostgreSQL.Entity.Types
  (
    -- * The /Entity/ Typeclass
    Entity (..)

    -- * Associated Types
  , Field (..)
  , UpdateRow(..)
  ) where

import Data.Vector (Vector)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))

-- | An 'Entity' stores the following information about the structure of a database table:
--
-- * Its name
-- * Its primary key
-- * The fields it contains
--
-- When using the functions provided by this library, you will need to provide
-- [Type Applications](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_applications.html)
-- in order to tell the compiler which 'Entity' you are referring to.
--
-- @since 0.0.1.0
class Entity e where
  -- | The name of the table in the PostgreSQL database.
  tableName  :: Text
  -- | The name of the primary key for the table.
  primaryKey :: Field
  -- | The fields of the table.
  fields     :: Vector Field

-- | A wrapper for table fields, with a very convenient 'IsString' instance.
--
-- @since 0.0.1.0
data Field
  = Field { fieldName :: Text
            -- ^ The name of the field in the database schema
          , fieldType :: Maybe Text
            -- ^ An optional postgresql type for which we need to be explicit, like @uuid[]@
          }
  deriving stock (Eq, Show)

-- | @since 0.0.1.0
instance IsString Field where
  fromString n = Field (toText n) Nothing

-- | Wrapper used by the update function in order to have the primary key as the last parameter passed,
-- since it appears in the WHERE clause.
--
-- @since 0.0.1.0
newtype UpdateRow a
  = UpdateRow { getUpdate :: a }
  deriving stock (Eq, Show)
  deriving newtype (Entity)

instance ToRow a => ToRow (UpdateRow a) where
  toRow = (drop <> take) 1 . toRow . getUpdate

