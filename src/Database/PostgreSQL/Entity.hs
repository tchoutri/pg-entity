{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}
{-|
  Module      : Database.PostgreSQL.Entity
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  A PostgreSQL database layer that does not get in your way.


  See the "Database.PostgreSQL.Entity.BlogPost" module for an example of a datatype implementing the 'Entity' typeclass
-}
module Database.PostgreSQL.Entity where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.ToField (ToField)

import Database.PostgreSQL.Entity.DBT (DBT, FromRow, Query, QueryNature (..), ToRow, execute, queryMany, queryOne)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Vector as V
-- >>> import Database.PostgreSQL.Entity
-- >>> import Database.PostgreSQL.Entity.BlogPost

-- | The main typeclass that is implemented by the data models.
class Entity e where
  -- | The name of the table in the PostgreSQL database.
  tableName  :: Text
  -- | The name of the primary key for the table.
  primaryKey :: Field
  -- | The fields of the table.
  fields     :: Vector Field

-- | A wrapper for table fields, with a very convenient 'IsString' instance.
newtype Field
  = Field { fieldName :: Text }
  deriving (Eq, Show)

instance IsString Field where
  fromString n = Field (toText n)

-- * High-level API

-- | Select an entity by its primary key.
selectById :: forall e value m.
           (Entity e, FromRow e, ToField value, MonadIO m)
           => value -> DBT m e
selectById value = selectOneByField (primaryKey @e) value

-- | Select precisely __one__ entity by a provided field.
selectOneByField :: forall e value m.
                 (Entity e, FromRow e, ToField value, MonadIO m)
                 => Field -> value -> DBT m e
selectOneByField f value = queryOne Select (_selectWhere @e [f]) (Only value)

-- | Select potentially many entities by a provided field.
selectManyByField :: forall e value m.
                  (Entity e, FromRow e, ToField value, MonadIO m)
                  => Field -> value -> DBT m (Vector e)
selectManyByField f value = queryMany Select (_selectWhere @e [f]) (Only value)

-- | Insert an entity.
insert :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => values -> DBT m ()
insert fs = void $ execute Insert (_insert @e) fs

-- | Delete an entity according to its primary key.
delete :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => values -> DBT m ()
delete values = void $ execute Delete (_delete @e) values

-- | Delete an entity cccording to a provided field.
deleteByField :: forall e values m.
       (Entity e, ToRow values, MonadIO m)
       => Vector Field -> values -> DBT m ()
deleteByField fs values = void $ execute Delete (_deleteWhere @e fs) values

-- * SQL keywords API

-- | Produce a SELECT expression for a given entity. 
--
-- __Examples__
--
-- >>> _select @BlogPost
-- "SELECT blogposts.blogpost_id, blogposts.author_id, blogposts.title, blogposts.content, blogposts.created_at FROM \"blogposts\""
_select :: forall e. Entity e => Query
_select = fromText $ "SELECT " <> expandQualifiedFields @e <> " FROM " <> quoteName (tableName @e)

-- | Produce a WHERE clause, given a vector of fields
-- It is most useful composed with a '_select' or '_delete', which is why these two combinations have their dedicated functions,
-- but the user is free to compose their own queries.
-- 
-- __Examples__ 
--
-- >>> _select @BlogPost <> _where ["blogpost_id"]
-- "SELECT blogposts.blogpost_id, blogposts.author_id, blogposts.title, blogposts.content, blogposts.created_at FROM \"blogposts\" WHERE \"blogpost_id\" = ?"
_where :: Vector Field -> Query
_where fs = fromText $ " WHERE " <> clauseFields
  where
    clauseFields = fold $ intercalateVector " AND " (fmap placeHolder fs)

-- | The composition of '_select' and '_where'. Nothing magical happens, it is just more convenient
--
-- __Examples__
--
-- >>> _selectWhere @BlogPost ["content"]
-- "SELECT blogposts.blogpost_id, blogposts.author_id, blogposts.title, blogposts.content, blogposts.created_at FROM \"blogposts\" WHERE \"content\" = ?"
_selectWhere :: forall e. Entity e => Vector Field -> Query
_selectWhere fs = _select @e <> _where fs

-- | Produce an INSERT statement for the given entity.
--
-- __Examples__
--
-- >>> _insert @BlogPost
-- "INSERT INTO \"blogposts\" (\"blogpost_id\", \"author_id\", \"title\", \"content\", \"created_at\") VALUES (?, ?, ?, ?, ?)"
_insert :: forall e. Entity e => Query
_insert = fromText $ "INSERT INTO " <> quoteName (tableName @e) <> " " <> fs <> " VALUES " <> ps
  where
    fs = inParens (expandFields @e)
    ps = inParens (generatePlaceholders $ fields @e)

-- | Produce a DELETE statement for the given entity, with a match on the Primary Key
--
-- __Examples__
--
-- >>> _delete @BlogPost
-- "DELETE FROM \"blogposts\" WHERE \"blogpost_id\" = ?"
_delete :: forall e. Entity e => Query
_delete = fromText ("DELETE FROM " <> quoteName (tableName @e)) <> _where [primaryKey @e]

-- | Produce a DELETE statement for the given entity, with a match on the desired fields
--
-- __Examples__
--
-- >>> _deleteWhere @BlogPost ["title", "createdAt"]
-- "DELETE FROM blogposts WHERE \"title\" = ? AND \"createdAt\" = ?"
_deleteWhere :: forall e. Entity e => Vector Field -> Query
_deleteWhere fs = fromText ("DELETE FROM " <> (tableName @e)) <> _where fs

-- * Helpers

-- | Wrap the given text between parentheses
--
-- __Examples__
--
-- >>> inParens "wrap me!"
-- "(wrap me!)"
inParens :: Text -> Text
inParens t = "(" <> t <> ")"

-- | Wrap the given text between double quotes
-- 
-- __Examples__
--
-- >>> quoteName "meow."
-- "\"meow.\""
quoteName :: Text -> Text
quoteName n = "\"" <> n <> "\""

-- | Produce a comma-separated list of an entity's fields.
-- 
-- __Examples__
--
-- >>> expandFields @BlogPost
-- "\"blogpost_id\", \"author_id\", \"title\", \"content\", \"created_at\""
expandFields :: forall e. Entity e => Text
expandFields = V.foldl1' (\element acc -> element <> ", " <> acc) (quoteName . fieldName <$> fields @e)

-- | Produce a comma-separated list of an entity's fields, qualified with the table name
--
-- __Examples__
--
-- >>> expandQualifiedFields @BlogPost
-- "blogposts.blogpost_id, blogposts.author_id, blogposts.title, blogposts.content, blogposts.created_at"
expandQualifiedFields :: forall e. Entity e => Text
expandQualifiedFields = expandQualifiedFields' @e prefix
  where
    prefix = tableName @e

-- | Produce a comma-separated list of an entity's 'fields', qualified with an arbitrary prefix
--
-- __Examples__
--
-- >>> expandQualifiedFields' @BlogPost "legacy"
-- "legacy.blogpost_id, legacy.author_id, legacy.title, legacy.content, legacy.created_at"
expandQualifiedFields' :: forall e. Entity e => Text -> Text
expandQualifiedFields' prefix = V.foldl1' (\element acc -> element <> ", " <> acc) fs
  where
    fs = fieldName <$> prefixFields prefix (fields @e)

-- | Take a prefix and a vector of fields, and qualifies each field with the prefix
--
-- __Examples__
--
-- >>> prefixFields "legacy" (fields @BlogPost)
-- [Field {fieldName = "legacy.blogpost_id"},Field {fieldName = "legacy.author_id"},Field {fieldName = "legacy.title"},Field {fieldName = "legacy.content"},Field {fieldName = "legacy.created_at"}]
prefixFields :: Text -> Vector Field -> Vector Field
prefixFields p fs = fmap (\(Field f) -> Field $ p <> "." <> f) fs

-- | Produce a placeholder of the form @\"field\" = ?@
--
-- __Examples__
--
-- >>> placeHolder "id"
-- "\"id\" = ?"
placeHolder :: Field -> Text
placeHolder (Field f) = quoteName f <> " = ?"

-- | Generate an appropriate number of '?' placeholders given a vector of fields
--
-- __Examples__
--
-- >>> generatePlaceholders ["id", "title", "content"]
-- "?, ?, ?"
generatePlaceholders :: Vector Field -> Text
generatePlaceholders vf = fold $ intercalateVector ", " $ fmap (const "?") vf

-- | Since the 'Query' type has an 'IsString' instance, the process of converting from 'Text' to 'String' to 'Query' is
-- factored into this function
fromText :: Text -> Query
fromText = fromString . toString

-- * Vector helpers

-- | The 'intercalateVector' function takes a Text and a Vector Text and concatenates the vector after interspersing
-- the first argument between each element of the list.
--
-- __Examples__
--
-- >>> intercalateVector "~" []
-- []
--
-- >>> intercalateVector "~" ["nyan"]
-- ["nyan"]
--
-- >>> intercalateVector "~" ["nyan", "nyan", "nyan"]
-- ["nyan","~","nyan","~","nyan"]
intercalateVector :: Text -> Vector Text -> Vector Text
intercalateVector sep vt | V.null vt = vt
                         | otherwise = V.cons x (go xs)
  where
    (x,xs) = (V.head vt, V.tail vt)
    go :: Vector Text -> Vector Text
    go ys | V.null ys = ys
          | otherwise = V.cons sep (V.cons (V.head ys) (go (V.tail ys)))
