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
  , Field
  , field
  , fieldName
  , fieldType
  , UpdateRow(..)

    -- * Generics
  , Options(..)
  , defaultEntityOptions

    -- * DerivingVia Options
  , GenericEntity(..)
  , EntityOptions(..)
  , PrimaryKey
  , Schema
  , TableName
  , FieldModifiers
  , TextModifier(..)
  , StripPrefix
  , CamelTo
  , CamelToSnake
  , CamelToKebab
  ) where

import Data.Char
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Manipulate as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Internal.Unsafe (Field (Field))
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
-- >   { key    :: Key
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
  -- | The name of the schema; will be appended to the table name: schema."tablename"
  schema :: Maybe Text
  schema = Nothing
  -- | The name of the primary key for the table.
  primaryKey :: Field
  default primaryKey :: (GetFields (Rep e)) => Field
  primaryKey = Field (primMod name) typ
    where primMod = primaryKeyModifiers defaultEntityOptions
          Field name typ = V.head $ getField @(Rep e) defaultEntityOptions
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
  getTableName Options{tableNameModifiers, fieldModifiers} = tableNameModifiers $ fieldModifiers $ T.pack $ symbolVal (Proxy :: Proxy name)

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
  getField Options{fieldModifiers} = V.singleton $ Field fieldName' Nothing
    where
      fieldName' = fieldModifiers $ T.pack $ symbolVal (Proxy @name)

-- Deriving Via machinery

newtype GenericEntity t e
  = GenericEntity { getGenericEntity :: e }

instance (EntityOptions t, GetTableName (Rep e), GetFields (Rep e)) => Entity (GenericEntity t e) where
  tableName = getTableName @(Rep e) (entityOptions @t)

  schema = schemaModifier (entityOptions @t)

  primaryKey = Field (primMod name) typ
    where primMod = primaryKeyModifiers defaultEntityOptions
          Field name typ = V.head $ getField @(Rep e) (entityOptions @t)

  fields = getField @(Rep e) (entityOptions @t)

-- | Term-level options
data Options
  = Options { tableNameModifiers  :: Text -> Text
            , schemaModifier      :: Maybe Text
            , primaryKeyModifiers :: Text -> Text
            , fieldModifiers      :: Text -> Text
            }

defaultEntityOptions :: Options
defaultEntityOptions = Options T.toSnake Nothing T.toSnake T.toSnake

-- | Type-level options for Deriving Via
class EntityOptions xs where
  entityOptions :: Options

instance EntityOptions '[] where
  entityOptions = defaultEntityOptions

instance (GetName name, EntityOptions xs) => EntityOptions (TableName name ': xs) where
  entityOptions = (entityOptions @xs){tableNameModifiers = const (getName @name)}

instance (GetName name, EntityOptions xs) => EntityOptions (PrimaryKey name ': xs) where
  entityOptions = (entityOptions @xs){primaryKeyModifiers = const (getName @name)}

instance (TextModifier mods, EntityOptions xs) => EntityOptions (FieldModifiers mods ': xs) where
  entityOptions = (entityOptions @xs){fieldModifiers = getTextModifier @mods}

instance (GetName name, EntityOptions xs) => EntityOptions (Schema name ': xs) where
  entityOptions = (entityOptions @xs){schemaModifier = Just $ getName @name}

data TableName (t :: Symbol)

data PrimaryKey (t :: Symbol)

data Schema (t :: Symbol)

-- | Contains a list of 'TextModifiers' modifiers
data FieldModifiers ms

-- | 'TextModifier' to remove a certain prefix from the fields
data StripPrefix (prefix :: Symbol)

-- | 'FieldModifier' taking a separator Char when transforming from CamelCase.
data CamelTo (separator :: Symbol)

-- | CamelCase to snake_case
type CamelToSnake = CamelTo "_"

-- | CamelCase to kebab-case
type CamelToKebab = CamelTo "-"

-- | The modifiers that you can apply to the fields:
--
-- * 'StripPrefix'
-- * 'CamelTo', and its variations
--   * 'CamelToSnake'
--   * 'CamelToKebab'
class TextModifier t where
  getTextModifier :: Text -> Text

--  No modifier
instance TextModifier '[] where
  getTextModifier = id

-- How we can have multiple modifiers chained
instance (TextModifier x, TextModifier xs) => TextModifier (x ': xs) where
  getTextModifier = getTextModifier @xs . getTextModifier @x

instance (KnownSymbol prefix) => TextModifier (StripPrefix prefix) where
  getTextModifier fld = fromMaybe fld (T.stripPrefix prefixToStrip fld)
    where
      prefixToStrip =  T.pack $ symbolVal (Proxy @prefix)

instance (KnownSymbol separator, NonEmptyText separator) => TextModifier (CamelTo separator) where
  getTextModifier fld = T.pack $ camelTo2 char (T.unpack fld)
    where
      char :: Char
      char = head $ symbolVal (Proxy @separator)
      camelTo2 :: Char -> String -> String
      camelTo2 c text = map toLower . go2 $ go1 text
          where go1 "" = ""
                go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
                go1 (x:xs) = x : go1 xs
                go2 "" = ""
                go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
                go2 (x:xs) = x : go2 xs


class GetName name where
  getName :: Text

instance (KnownSymbol name, NonEmptyText name) => GetName name where
  getName = T.pack (symbolVal (Proxy @name))

type family NonEmptyText (xs :: Symbol) :: Constraint where
  NonEmptyText "" = TypeError ('Text "User-provided string cannot be empty!")
  NonEmptyText _  = ()

-- | Get the name of a field.
--
-- @since 0.1.0.0
fieldName :: Field -> Text
fieldName (Field name _) = name

-- | Get the type of a field, if any.
--
-- @since 0.1.0.0
fieldType :: Field -> Maybe Text
fieldType (Field _ typ) = typ

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
