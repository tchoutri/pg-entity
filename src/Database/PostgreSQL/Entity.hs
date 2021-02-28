{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
{-|
  Module      : Database.PostgreSQL.Entity
  Copyright   : (c) Clément Delafargue, 2018
                    Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  A PostgreSQL database layer that does not get in your way
-}
module Database.PostgreSQL.Entity where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.ToField (ToField)

import Database.PostgreSQL.Entity.DBT (DBT, FromRow, Query, QueryNature (..), ToRow, execute, queryMany, queryOne)

{-| The main typeclass that is implemented by the data models.
-}
class Entity e where
  tableName  :: Text
  primaryKey :: Field
  fields     :: Vector Field

newtype Field
  = Field { fieldName :: Text }
  deriving (Eq, Show)

instance IsString Field where
  fromString n = Field (toText n)

-- * High-level API
selectById :: forall e value m.
           (Entity e, FromRow e, ToField value, MonadIO m)
           => value -> DBT m e
selectById value = selectOneByField (primaryKey @e) value

selectOneByField :: forall e value m.
                 (Entity e, FromRow e, ToField value, MonadIO m)
                 => Field -> value -> DBT m e
selectOneByField f value = queryOne Select (_selectWhere @e [f]) (Only value)

selectManyByField :: forall e value m.
                  (Entity e, FromRow e, ToField value, MonadIO m)
                  => Field -> value -> DBT m (Vector e)
selectManyByField f value = queryMany Select (_selectWhere @e [f]) (Only value)

insert :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => values -> DBT m ()
insert fs = void $ execute Insert (_insert @e) fs

delete :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => values -> DBT m ()
delete values = void $ execute Delete (_delete @e) values

deleteByField :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => Vector Field -> values -> DBT m ()
deleteByField fs values = void $ execute Delete (_deleteWhere @e fs) values

-- * SQL keywords API

_select :: forall e. Entity e => Query
_select = fromText $ "SELECT " <> expandQualifiedFields @e <> " FROM " <> quoteName (tableName @e)
{-# INLINEABLE _select #-}

_where :: Vector Field -> Query
_where fs = fromText $ " WHERE " <> clauseFields
  where
    clauseFields = fold $ intercalateVector " AND " (fmap placeHolder fs)

_selectWhere :: forall e. Entity e => Vector Field -> Query
_selectWhere fs = _select @e <> _where fs

_insert :: forall e. Entity e => Query
_insert = fromText $ "INSERT INTO " <> tableName @e <> " " <> fs <> " VALUES " <> ps
  where
    fs = inParens (expandFields @e)
    ps = inParens (generatePlaceholders $ fields @e)

_delete :: forall e. Entity e => Query
_delete = fromText ("DELETE FROM " <> (tableName @e)) <> _where [primaryKey @e]

_deleteWhere :: forall e. Entity e => Vector Field -> Query
_deleteWhere fs = fromText ("DELETE FROM " <> (tableName @e)) <> _where fs

-- * Helpers

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

fieldsFromList :: [Text] -> Vector Field
fieldsFromList list = Field <$> V.fromList list

quoteName :: Text -> Text
quoteName n = "\"" <> n <> "\""

expandFields :: forall e. Entity e => Text
expandFields = V.foldl1' (\element acc -> element <> ", " <> acc) (fieldName <$> fields @e)

expandQualifiedFields :: forall e. Entity e => Text
expandQualifiedFields = expandQualifiedFields' @e prefix
  where
    prefix = tableName @e

expandQualifiedFields' :: forall e. Entity e => Text -> Text
expandQualifiedFields' prefix = V.foldl1' (\element acc -> element <> ", " <> acc) fs
  where
    fs = fieldName <$> prefixFields prefix (fields @e)

prefixFields :: Text -> Vector Field -> Vector Field
prefixFields p fs = fmap (\(Field f) -> Field $ p <> "." <> f) fs

placeHolder :: Field -> Text
placeHolder (Field f) = quoteName f <> " = ?"

generatePlaceholders :: Vector Field -> Text
generatePlaceholders vf = fold $ intercalateVector ", " $ fmap (const "?") vf

fromText :: Text -> Query
fromText = fromString . toString

-- * Vector helpers

intercalateVector :: Text -> Vector Text -> Vector Text
intercalateVector sep vt | V.null vt = vt
                         | otherwise = V.cons x (go xs)
  where
    (x,xs) = (V.head vt, V.tail vt)
    go :: Vector Text -> Vector Text
    go ys | V.null ys = ys
          | otherwise = V.cons sep (V.cons (V.head ys) (go (V.tail ys)))
