{-|
  Module      : Database.PostgreSQL.Entity.Types
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Types and classes

-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.PostgreSQL.Entity.Types
  (
    -- * The /Entity/ Typeclass
    Entity (..)

    -- * Associated Types
  , Field (..)
  , UpdateRow(..)

    -- * Generics
  , Options(..)
  , defaultEntityOptions

    -- * DerivingVia Options
  , GenericEntity(..)
  , EntityOptions(..)
  , PrimaryKey
  , TableName
  ) where

import Data.Kind
import Data.Proxy
import Data.Text (Text, pack)
import qualified Data.Text.Manipulate as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import GHC.Generics
import GHC.TypeLits

-- | An 'Entity' stores the following information about the structure of a database table:
--
-- * Its name
-- * Its primary key
-- * The fields it contains
--
-- == Example
--
-- > data ExampleEntity = E
-- >   { key :: Key
-- >   , field1 :: Int
-- >   , field2 :: Bool
-- >   }
-- >   deriving stock (Eq, Show, Generic)
-- >   deriving anyclass (FromRow, ToRow)
-- >   deriving Entity
-- >      via (GenericEntity '[TableName "entities"] ExampleEntity)
--
-- When using the functions provided by this library, you will sometimes need to be explicit about the Entity you are
-- referring to.
--
-- @since 0.0.1.0
class Entity e where
  -- | The name of the table in the PostgreSQL database.
  tableName :: Text
  default tableName :: (GetTableName (Rep e)) => Text
  tableName = getTableName @(Rep e) defaultEntityOptions
  -- | The name of the primary key for the table.
  primaryKey :: Field
  default primaryKey :: (GetFields (Rep e)) => Field
  primaryKey = field{fieldName = primMod $ fieldName field}
    where primMod = primaryKeyModifier defaultEntityOptions
          field = V.head $ getField @(Rep e) defaultEntityOptions
  -- | The fields of the table.
  fields :: Vector Field
  default fields :: (GetFields (Rep e)) => Vector Field
  fields = getField @(Rep e) defaultEntityOptions

-- The sub-class that fetches the table name
class GetTableName (e :: Type -> Type) where
  getTableName :: Options -> Text

instance (TypeError ('Text "You can't derive Entity for a void type")) => GetTableName V1 where
  getTableName _opts = error "You can't derive Entity for a void type"

instance (TypeError ('Text "You can't derive Entity for a unit type")) => GetTableName U1 where
  getTableName _opts = error "You can't derive Entity for a unit type"

instance (TypeError ('Text "You can't derive Entity for a sum type")) => GetTableName (e :+: f) where
  getTableName _opts = error "You can't derive Entity for a sum type"

instance (TypeError ('Text "You can't derive an Entity for a type constructor's field")) => GetTableName (K1 i c) where
  getTableName _opts = error "You can't derive Entity for a type constructor's field"

instance (TypeError ('Text "You don't have to derive GetTableName for a product type")) => GetTableName (e :*: f) where
  getTableName _opts = error "You don't have to derive GetTableName for a product type"

instance GetTableName e => GetTableName (M1 C _1 e) where
  getTableName opts = getTableName @e opts

instance GetTableName e => GetTableName (M1 S _1 e) where
  getTableName opts = getTableName @e opts

instance (KnownSymbol name)
    => GetTableName (M1 D ('MetaData name _1 _2 _3) e) where
  getTableName Options{tableNameModifier} = tableNameModifier $ pack $ symbolVal (Proxy :: Proxy name)

-- The sub-class that fetches the table fields
class GetFields (e :: Type -> Type) where
  getField :: Options -> Vector Field

instance (TypeError ('Text "You can't derive Entity for a void type")) => GetFields V1 where
  getField _opts = error "You can't derive Entity for a void type"

instance (TypeError ('Text "You can't derive Entity for a unit type")) => GetFields U1 where
  getField _opts = error "You can't derive Entity for a unit type"

instance (TypeError ('Text "You can't derive Entity for a sum type")) => GetFields (e :+: f) where
  getField _opts = error "You can't derive Entity for a sum type"

instance (TypeError ('Text "You can't derive Entity for a a type constructor's field")) => GetFields (K1 i c) where
  getField _opts = error "You can't derive Entity for a type constructor's field"

instance (GetFields e, GetFields f) => GetFields (e :*: f) where
  getField opts = getField @e opts <> getField @f opts

instance GetFields e => GetFields (M1 C _1 e) where
  getField opts = getField @e opts

instance GetFields e => GetFields (M1 D ('MetaData _1 _2 _3 _4) e) where
  getField opts = getField @e opts

instance (KnownSymbol name) => GetFields (M1 S ('MetaSel ('Just name) _1 _2 _3) _4) where
  getField Options{fieldModifier} = V.singleton $ Field fieldName Nothing
    where fieldName = fieldModifier $ pack $ symbolVal (Proxy @name)

-- Deriving Via machinery

newtype GenericEntity t e
  = GenericEntity { getGenericEntity :: e }

instance (EntityOptions t, GetTableName (Rep e), GetFields (Rep e)) => Entity (GenericEntity t e) where
  tableName = getTableName @(Rep e) (entityOptions @t)

  primaryKey = field{fieldName = primMod $ fieldName field}
    where primMod = primaryKeyModifier defaultEntityOptions
          field = V.head $ getField @(Rep e) (entityOptions @t)

  fields = getField @(Rep e) (entityOptions @t)

-- | Term-level options
data Options
  = Options { tableNameModifier  :: Text -> Text
            , primaryKeyModifier :: Text -> Text
            , fieldModifier      :: Text -> Text
            }

defaultEntityOptions :: Options
defaultEntityOptions = Options T.toSnake T.toSnake T.toSnake

-- | Type-level options for Deriving Via
class EntityOptions xs where
  entityOptions :: Options

instance EntityOptions '[] where
  entityOptions = defaultEntityOptions

instance (GetName name, EntityOptions xs) => EntityOptions (TableName name ': xs) where
  entityOptions = (entityOptions @xs){tableNameModifier = const (getName @name)}

instance (GetName name, EntityOptions xs) => EntityOptions (PrimaryKey name ': xs) where
  entityOptions = (entityOptions @xs){primaryKeyModifier = const (getName @name)}

data TableName t

data PrimaryKey t

class GetName name where
  getName :: Text

instance (KnownSymbol name, NonEmptyText name) => GetName name where
  getName = pack (symbolVal (Proxy @name))

type family NonEmptyText (xs :: Symbol) :: Constraint where
  NonEmptyText "" = TypeError ('Text "User-provided string cannot be empty!")
  NonEmptyText _  = ()

-- | A wrapper for table fields, with a very convenient 'IsString' instance.
--
-- === __Example:__
--
-- > instance Entity BlogPost where
-- >   tableName  = "blogposts"
-- >   primaryKey = "blogpost_id"
-- >   fields = [ "blogpost_id"
-- >            , "author_id"
-- >            , "uuid_list" `withType` "uuid[]" -- ← This is where we specify an optional PostgreSQL type annotation
-- >            , "title"
-- >            , "content"
-- >            , "created_at"
-- >            ]
--
-- @since 0.0.1.0
data Field
  = Field { fieldName :: Text
            -- ^ The name of the field in the database schema
          , fieldType :: Maybe Text
            -- ^ An optional postgresql type for which we need to be explicit, like @Just "uuid[]"@
          }
  deriving stock (Eq, Generic, Show)

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

