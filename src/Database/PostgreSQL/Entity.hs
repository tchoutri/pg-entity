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

  See the "Database.PostgreSQL.Entity.Internal.BlogPost" module for an example of a data-type implementing the 'Entity' typeclass.
-}
module Database.PostgreSQL.Entity
  ( -- * The /Entity/ Typeclass
    Entity (..)

    -- * Associated Types
  , Field

    -- * High-level API
    -- $highlevel

    -- ** Selection
  , selectById
  , selectOneByField
  , selectManyByField
  , selectWhereNotNull
  , selectWhereNull
  , selectOneWhereIn
  , joinSelectById
  , joinSelectOneByField
  , selectOrderBy

    -- ** Insertion
  , insert
  , insertMany
  , upsert
  , upsertOnPK_

    -- ** Update
  , update
  , updateFieldsBy

    -- ** Deletion
  , delete
  , deleteByField

    -- * SQL Combinators API

    -- ** Selection
  , _select
  , _selectWithFields
  , _where
  , _selectWhere
  , _selectWhereNotNull
  , _selectWhereNull
  , _selectWhereIn
  , _joinSelect
  , _innerJoin
  , _joinSelectWithFields
  , _joinSelectOneByField

    -- ** Insertion
  , _insert
  , _onConflictDoUpdate
  , _onConflictDoNothing

    -- ** Update
  , _update
  , _updateBy
  , _updateFields
  , _updateFieldsBy

    -- ** Deletion
  , _delete
  , _deleteWhere
  , _orderBy
  , _orderByMany
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (fold)
import Data.Int (Int64)
import Data.Text.Display (display)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import Database.PostgreSQL.Transact (DBT)

import Data.Text (Text)
import Database.PostgreSQL.Entity.DBT (execute, executeMany, query, queryOne, queryOne_, query_)
import Database.PostgreSQL.Entity.Internal
import Database.PostgreSQL.Entity.Types

{- $setup
>>> :set -XQuasiQuotes
>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> :set -XTypeApplications
>>> import Database.PostgreSQL.Entity
>>> import Database.PostgreSQL.Entity.Types
>>> import Database.PostgreSQL.Entity.Internal
>>> import Database.PostgreSQL.Entity.Internal.QQ
>>> import Database.PostgreSQL.Simple.Types (Query (..))
>>> import Data.Vector (Vector)
>>> import qualified Data.Vector as V
>>> import Data.Time (UTCTime)
>>> import GHC.Generics (Generic)
>>> :{
>>> data Author = Author
>>>   { authorId :: Int
>>>   , name :: Text
>>>   , createdAt :: UTCTime
>>>   }
>>>   deriving stock (Eq, Generic, Ord, Show)
>>>   deriving anyclass (FromRow, ToRow)
>>>   deriving
>>>     (Entity)
>>>     via (GenericEntity '[PrimaryKey "author_id", TableName "authors"] Author)
>>>
>>> data BlogPost = BlogPost
>>>   { blogPostId :: Int
>>>   -- ^ Primary key
>>>   , authorId :: Int
>>>   -- ^ Foreign keys, for which we need an explicit type annotation
>>>   , intList :: Vector Int
>>>   , title :: Text
>>>   , content :: Text
>>>   , createdAt :: UTCTime
>>>   }
>>>   deriving stock (Eq, Generic, Ord, Show)
>>>   deriving anyclass (FromRow, ToRow)
>>>   deriving
>>>     (Entity)
>>>     via (GenericEntity '[PrimaryKey "blog_post_id", TableName "blogposts"] BlogPost)
>>> :}
-}

{- $highlevel
 Glossary / Tips’n’Tricks

 * @e@, @e1@, @e2@: Represents an @Entity@
 * @value@: Represents a Haskell value that can be serialised to PostgreSQL
 * @Field@: Parameters of type @Field@ can most often be passed in their textual form inside the 'field' quasi-quoter,
   like @[field| author_id :: uuid|]@. This metaprogramming technique is here to better prevent empty fields from being passed.
   The PostgreSQL type annotation is optional, but necessary for arrays of UUIDs and of custom enums.

 Consult the [test suite](https://github.com/tchoutri/pg-entity/tree/main/test) to see those functions in action.
-}

{-| Select an entity by its primary key.

 @since 0.0.1.0
-}
selectById
  :: forall e value m
   . (Entity e, FromRow e, MonadIO m, ToRow value)
  => value
  -> DBT m (Maybe e)
selectById value = selectOneByField (primaryKey @e) value

{-| Select precisely __one__ entity by a provided field.

 @since 0.0.1.0
-}
selectOneByField
  :: forall e value m
   . (Entity e, FromRow e, MonadIO m, ToRow value)
  => Field
  -> value
  -> DBT m (Maybe e)
selectOneByField f value = queryOne (_selectWhere @e [f]) value

{-| Select potentially many entities by a provided field.

 @since 0.0.1.0
-}
selectManyByField
  :: forall e value m
   . (Entity e, FromRow e, MonadIO m, ToRow value)
  => Field
  -> value
  -> DBT m (Vector e)
selectManyByField f value = query (_selectWhere @e [f]) value

{-| Select statement with a non-null condition

 See '_selectWhereNotNull' for the generated query.

 @since 0.0.1.0
-}
selectWhereNotNull
  :: forall e m
   . (Entity e, FromRow e, MonadIO m)
  => Vector Field
  -> DBT m (Vector e)
selectWhereNotNull fs = query_ (_selectWhereNotNull @e fs)

{-| Select statement with a null condition

 See '_selectWhereNull' for the generated query.

 @since 0.0.1.0
-}
selectWhereNull
  :: forall e m
   . (Entity e, FromRow e, MonadIO m)
  => Vector Field
  -> DBT m (Vector e)
selectWhereNull fs = query_ (_selectWhereNull @e fs)

{-| Select statement when for an entity where the field is one of the options passed

 @since 0.0.2.0
-}
selectOneWhereIn
  :: forall e m
   . (Entity e, FromRow e, MonadIO m)
  => Field
  -> Vector Text
  -> DBT m (Maybe e)
selectOneWhereIn f values = queryOne_ (_selectWhereIn @e f values)

{-| Perform a INNER JOIN between two entities

 @since 0.0.1.0
-}
joinSelectById
  :: forall e1 e2 m
   . (Entity e1, Entity e2, FromRow e1, MonadIO m)
  => DBT m (Vector e1)
joinSelectById = query_ (_joinSelect @e1 @e2)

{-| Perform a @INNER JOIN ON field1 WHERE field2 = value@ between two entities

 @since 0.0.2.0
-}
joinSelectOneByField
  :: forall e1 e2 value m
   . (Entity e1, Entity e2, FromRow e1, MonadIO m, ToField value)
  => Field
  -- ^ The field over which the two tables will be joined
  -> Field
  -- ^ The field in the where clause
  -> value
  -- ^ The value of the where clause
  -> DBT m (Vector e1)
joinSelectOneByField pivot whereClause value =
  query (_joinSelectOneByField @e1 @e2 pivot whereClause) (Only value)

--

{-| Perform a SELECT + ORDER BY query on an entity

 @since 0.0.2.0
-}
selectOrderBy
  :: forall e m
   . (Entity e, FromRow e, MonadIO m)
  => Vector (Field, SortKeyword)
  -> DBT m (Vector e)
selectOrderBy sortSpec = query_ (_select @e <> _orderByMany sortSpec)

{-| Insert an entity.

 @since 0.0.1.0
-}
insert
  :: forall e values m
   . (Entity e, ToRow values, MonadIO m)
  => values
  -> DBT m ()
insert fs = void $ execute (_insert @e) fs

{-| Insert an entity with an "ON CONFLICT DO UPDATE" clause on the primary key as the conflict target

 @since 0.0.2.0
-}
upsert
  :: forall e values m
   . (Entity e, ToRow values, MonadIO m)
  => values
  -- ^ Entity to insert
  -> Vector Field
  -- ^ Fields to replace in case of conflict
  -> DBT m ()
upsert entity fieldsToReplace = void $ execute (_insert @e <> _onConflictDoUpdate conflictTarget fieldsToReplace) entity
  where
    conflictTarget = V.singleton $ primaryKey @e

{-| Insert an entity with an "ON CONFLICT DO NOTHING" clause

 @since 0.0.2.0
-}
upsertOnPK_
  :: forall e values m
   . (Entity e, ToRow values, MonadIO m)
  => values
  -- ^ Entity to insert
  -> DBT m ()
upsertOnPK_ entity = void $ execute (_insert @e <> _onConflictDoNothing conflictTarget ) entity
  where
    conflictTarget = V.singleton $ primaryKey @e

{-| Insert multiple rows of an entity.

 @since 0.0.2.0
-}
insertMany
  :: forall e values m
   . (Entity e, ToRow values, MonadIO m)
  => [values]
  -> DBT m ()
insertMany values = void $ executeMany (_insert @e) values

{-| Update an entity.

 The Id of the entity is put at the end of the query automatically through the use of 'UpdateRow'.
 __Examples__

 > let newAuthor = oldAuthor{…}
 > update @Author newAuthor

 @since 0.0.1.0
-}
update
  :: forall e newValue m
   . (Entity e, ToRow newValue, MonadIO m)
  => newValue
  -> DBT m ()
update fs = void $ execute (_update @e) (UpdateRow fs)

{-| Update rows of an entity matching the given value

 == Example

 > let newName = "Tiberus McElroy" :: Text
 > let oldName = "Johnson McElroy" :: Text
 > updateFieldsBy @Author [[field| name |]] ([field| name |], oldName) (Only newName)

 @since 0.0.1.0
-}
updateFieldsBy
  :: forall e v1 v2 m
   . (Entity e, MonadIO m, ToRow v2, ToField v1)
  => Vector Field
  -- ^ Fields to change
  -> (Field, v1)
  -- ^ Field on which to match and its value
  -> v2
  -- ^ New values of those fields
  -> DBT m Int64
updateFieldsBy fs (f, oldValue) newValue = execute (_updateFieldsBy @e fs f) (toRow newValue ++ toRow (Only oldValue))

{-| Delete an entity according to its primary key.

 @since 0.0.1.0
-}
delete
  :: forall e value m
   . (Entity e, ToRow value, MonadIO m)
  => value
  -> DBT m ()
delete value = deleteByField @e [primaryKey @e] value

{-| Delete rows according to the given fields

 == Example

 > deleteByField @BlogPost [[field| title |]] (Only "Echoes from the other world")

 @since 0.0.1.0
-}
deleteByField
  :: forall e values m
   . (Entity e, ToRow values, MonadIO m)
  => Vector Field
  -> values
  -> DBT m ()
deleteByField fs values = void $ execute (_deleteWhere @e fs) values

-- * SQL combinators API

{-| Produce a SELECT statement for a given entity.

 __Examples__

 >>> _select @BlogPost
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\""

 @since 0.0.1.0
-}
_select :: forall e. Entity e => Query
_select = textToQuery $ "SELECT " <> expandQualifiedFields @e <> " FROM " <> getTableName @e

{-| Produce a SELECT statement with explicit fields for a given entity

 __Examples__

 >>> _selectWithFields @BlogPost [ [field| blog_post_id |], [field| created_at |] ]
 "SELECT \"blogposts\".\"blog_post_id\", \"blogposts\".\"created_at\" FROM \"blogposts\""

 @since 0.0.1.0
-}
_selectWithFields :: forall e. Entity e => Vector Field -> Query
_selectWithFields fs = textToQuery $ "SELECT " <> expandQualifiedFields' fs tn <> " FROM " <> tn
  where
    tn = getTableName @e

{-| Produce a WHERE clause, given a vector of fields.

 It is most useful composed with a '_select' or '_delete', which is why these two combinations have their dedicated functions,
 but the user is free to compose their own queries.

 The 'Entity' constraint is required for '_where' in order to get any type annotation that was given in the schema.
 Fields that do not exist in the Entity will be kept so that PostgreSQL can report the error.

 __Examples__

 >>> _select @BlogPost <> _where [[field| blog_post_id |]]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"blog_post_id\" = ?"

 >>> _select @BlogPost <> _where [ [field| int_list |] ]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"int_list\" = ?"

 @since 0.0.1.0
-}
_where :: Vector Field -> Query
_where fs' = textToQuery $ " WHERE " <> clauseFields
  where
    clauseFields = fold $ intercalateVector " AND " (fmap placeholder fs')

{-| Produce a SELECT statement for a given entity and fields.

 __Examples__

 >>> _selectWhere @BlogPost [ [field| author_id |] ]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"author_id\" = ?"

 >>> _selectWhere @BlogPost [ [field| author_id |], [field| title |]]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"author_id\" = ? AND \"title\" = ?"

 @since 0.0.1.0
-}
_selectWhere :: forall e. Entity e => Vector Field -> Query
_selectWhere fs = _select @e <> _where fs

{-| Produce a SELECT statement where the provided fields are checked for being non-null.
 r

 >>> _selectWhereNotNull @BlogPost [ [field| author_id |] ]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"author_id\" IS NOT NULL"

 @since 0.0.1.0
-}
_selectWhereNotNull :: forall e. Entity e => Vector Field -> Query
_selectWhereNotNull fs = _select @e <> textToQuery (" WHERE " <> isNotNull fs)

{-| Produce a SELECT statement where the provided fields are checked for being null.

 >>> _selectWhereNull @BlogPost [ [field| author_id |] ]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"author_id\" IS NULL"

 @since 0.0.1.0
-}
_selectWhereNull :: forall e. Entity e => Vector Field -> Query
_selectWhereNull fs = _select @e <> textToQuery (" WHERE " <> isNull fs)

{-| Produce a SELECT statement where the given field is checked aginst the provided array of values .

 >>> _selectWhereIn @BlogPost [field| title |] [ "Unnamed", "Mordred's Song" ]
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" WHERE \"title\" IN ('Unnamed', 'Mordred''s Song')"

 @since 0.0.2.0
-}
_selectWhereIn :: forall e. Entity e => Field -> Vector Text -> Query
_selectWhereIn f values = _select @e <> textToQuery (" WHERE " <> isIn f values)

{-| Produce a "SELECT FROM" over two entities.

 __Examples__

 >>> _joinSelect @BlogPost @Author
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\", authors.\"author_id\", authors.\"name\", authors.\"created_at\" FROM \"blogposts\" INNER JOIN \"authors\" USING(author_id)"

 @since 0.0.1.0
-}
_joinSelect :: forall e1 e2. (Entity e1, Entity e2) => Query
_joinSelect =
  textToQuery $
    "SELECT "
      <> expandQualifiedFields @e1
      <> ", "
      <> expandQualifiedFields @e2
      <> " FROM "
      <> getTableName @e1
      <> queryToText (_innerJoin @e2 (primaryKey @e2))

{-| Produce a "INNER JOIN … USING(…)" fragment.

 __Examples__

 >>> _innerJoin @BlogPost [field| author_id |]
 " INNER JOIN \"blogposts\" USING(author_id)"

 @since 0.0.1.0
-}
_innerJoin :: forall e. Entity e => Field -> Query
_innerJoin f =
  textToQuery $
    " INNER JOIN "
      <> getTableName @e
      <> " USING("
      <> fieldName f
      <> ")"

{-| Produce a "SELECT [table1_fields, table2_fields] FROM table1 INNER JOIN table2 USING(table2_pk)" statement.
 The primary is used as the join point between the two tables.

 __Examples__

 >>> _joinSelectWithFields @BlogPost @Author [ [field| title |] ] [ [field| name |] ]
 "SELECT \"blogposts\".\"title\", \"authors\".\"name\" FROM \"blogposts\" INNER JOIN \"authors\" USING(author_id)"

 @since 0.0.1.0
-}
_joinSelectWithFields
  :: forall e1 e2
   . (Entity e1, Entity e2)
  => Vector Field
  -> Vector Field
  -> Query
_joinSelectWithFields fs1 fs2 =
  textToQuery $
    "SELECT "
      <> expandQualifiedFields' fs1 tn1
      <> ", "
      <> expandQualifiedFields' fs2 tn2
      <> " FROM "
      <> getTableName @e1
      <> queryToText (_innerJoin @e2 (primaryKey @e2))
  where
    tn1 = getTableName @e1
    tn2 = getTableName @e2

{-| Produce a "SELECT FROM" over two entities.

 __Examples__

 >>> _joinSelectOneByField @BlogPost @Author [field| author_id |] [field| name |] :: Query
 "SELECT blogposts.\"blog_post_id\", blogposts.\"author_id\", blogposts.\"int_list\", blogposts.\"title\", blogposts.\"content\", blogposts.\"created_at\" FROM \"blogposts\" INNER JOIN \"authors\" ON \"blogposts\".\"author_id\" = \"authors\".\"author_id\" WHERE authors.\"name\" = ?"

 @since 0.0.2.0
-}
_joinSelectOneByField
  :: forall e1 e2
   . (Entity e1, Entity e2)
  => Field
  -> Field
  -> Query
_joinSelectOneByField pivotField whereField =
  textToQuery $
    "SELECT "
      <> expandQualifiedFields @e1
      <> " FROM "
      <> (getTableName @e1)
      <> " INNER JOIN "
      <> getTableName @e2
      <> " ON "
      <> (getTableName @e1)
      <> "."
      <> getFieldName pivotField
      <> " = "
      <> (getTableName @e2)
      <> "."
      <> getFieldName pivotField
      <> " WHERE "
      <> placeholder' @e2 whereField

{-| Produce an INSERT statement for the given entity.

 __Examples__

 >>> _insert @BlogPost
 "INSERT INTO \"blogposts\" (\"blog_post_id\", \"author_id\", \"int_list\", \"title\", \"content\", \"created_at\") VALUES (?, ?, ?, ?, ?, ?)"

 @since 0.0.1.0
-}
_insert :: forall e. Entity e => Query
_insert = textToQuery $ "INSERT INTO " <> getTableName @e <> " " <> fs <> " VALUES " <> ps
  where
    fs = inParens (expandFields @e)
    ps = inParens (generatePlaceholders $ fields @e)

{-| Produce a "ON CONFLICT (target) DO UPDATE SET …" statement.

 __Examples__

 >>> _onConflictDoUpdate [[field| blog_post_id |]] [ [field| title |], [field| content |]]
 " ON CONFLICT (blog_post_id) DO UPDATE SET title = EXCLUDED.title, content = EXCLUDED.content"

 >>> _onConflictDoUpdate [[field| blog_post_id |], [field| author_id |]] [ [field| title |], [field| content |]]
 " ON CONFLICT (blog_post_id, author_id) DO UPDATE SET title = EXCLUDED.title, content = EXCLUDED.content"

 >>> _insert @BlogPost <> _onConflictDoUpdate [[field| blog_post_id |]] [ [field| title |], [field| content |]]
 "INSERT INTO \"blogposts\" (\"blog_post_id\", \"author_id\", \"int_list\", \"title\", \"content\", \"created_at\") VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (blog_post_id) DO UPDATE SET title = EXCLUDED.title, content = EXCLUDED.content"

 @since 0.0.2.0
-}
_onConflictDoUpdate :: Vector Field -> Vector Field -> Query
_onConflictDoUpdate conflictTarget fieldsToReplace =
  textToQuery $ " ON CONFLICT (" <> targetNames <> ") DO UPDATE SET " <> replacedFields
  where
    targetNames = fold $ intercalateVector ", " (fmap fieldName conflictTarget)
    replacedFields = fold $ intercalateVector ", " (fmap (replaceField . fieldName) fieldsToReplace)
    replaceField :: Text -> Text
    replaceField f = f <> " = EXCLUDED." <> f

-- | Produce a "ON CONFLICT (target) DO NOTHING" statement.
--
-- __Examples__
--
-- >>> _onConflictDoNothing [[field| blog_post_id |]]
-- " ON CONFLICT (blog_post_id) DO NOTHING"
--
-- >>> _onConflictDoNothing [[field| blog_post_id |], [field| author_id |]]
-- " ON CONFLICT (blog_post_id, author_id) DO NOTHING"
--
-- >>> _insert @BlogPost <> _onConflictDoNothing [[field| blog_post_id |]]
-- "INSERT INTO \"blogposts\" (\"blog_post_id\", \"author_id\", \"int_list\", \"title\", \"content\", \"created_at\") VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (blog_post_id) DO NOTHING"
--
-- @since 0.0.2.0
_onConflictDoNothing :: Vector Field -> Query
_onConflictDoNothing conflictTarget =
  textToQuery $ " ON CONFLICT (" <> targetNames <> ") DO NOTHING"
  where
    targetNames = fold $ intercalateVector ", " (fmap fieldName conflictTarget)

{-| Produce an UPDATE statement for the given entity by primary key

 __Examples__

 >>> _update @Author
 "UPDATE \"authors\" SET (\"name\", \"created_at\") = ROW(?, ?) WHERE \"author_id\" = ?"

 >>> _update @BlogPost
 "UPDATE \"blogposts\" SET (\"author_id\", \"int_list\", \"title\", \"content\", \"created_at\") = ROW(?, ?, ?, ?, ?) WHERE \"blog_post_id\" = ?"

 @since 0.0.1.0
-}
_update :: forall e. Entity e => Query
_update = _updateBy @e (primaryKey @e)

{-| Produce an UPDATE statement for the given entity by the given field.

 __Examples__

 >>> _updateBy @Author [field| name |]
 "UPDATE \"authors\" SET (\"name\", \"created_at\") = ROW(?, ?) WHERE \"name\" = ?"

 @since 0.0.1.0
-}
_updateBy :: forall e. Entity e => Field -> Query
_updateBy f = _updateFieldsBy @e (fields @e) f

{-| Produce an UPDATE statement for the given entity and fields, by primary key.

 >>> _updateFields @Author [ [field| name |] ]
 "UPDATE \"authors\" SET (\"name\") = ROW(?) WHERE \"author_id\" = ?"

 @since 0.0.1.0
-}
_updateFields :: forall e. Entity e => Vector Field -> Query
_updateFields fs = _updateFieldsBy @e fs (primaryKey @e)

{-| Produce an UPDATE statement for the given entity and fields, by the specified field.

 >>> _updateFieldsBy @Author [ [field| name |] ] [field| name |]
 "UPDATE \"authors\" SET (\"name\") = ROW(?) WHERE \"name\" = ?"

 >>> _updateFieldsBy @BlogPost [[field| author_id |], [field| title |]] [field| title |]
 "UPDATE \"blogposts\" SET (\"author_id\", \"title\") = ROW(?, ?) WHERE \"title\" = ?"

 @since 0.0.1.0
-}
_updateFieldsBy
  :: forall e
   . Entity e
  => Vector Field
  -- ^ Field names to update
  -> Field
  -- ^ Field on which to match
  -> Query
_updateFieldsBy fs' f =
  textToQuery
    ( "UPDATE "
        <> getTableName @e
        <> " SET "
        <> updatedFields
        <> " = "
        <> newValues
    )
    <> _where [f]
  where
    fs = V.filter (/= (primaryKey @e)) fs'
    newValues = "ROW" <> inParens (generatePlaceholders fs)
    updatedFields =
      inParens $
        V.foldl1' (\element acc -> element <> ", " <> acc) (quoteName . fieldName <$> fs)

{-| Produce a DELETE statement for the given entity, with a match on the Primary Key

 __Examples__

 >>> _delete @BlogPost
 "DELETE FROM \"blogposts\" WHERE \"blog_post_id\" = ?"

 @since 0.0.1.0
-}
_delete :: forall e. Entity e => Query
_delete = textToQuery ("DELETE FROM " <> getTableName @e) <> _where [primaryKey @e]

{-| Produce a DELETE statement for the given entity and fields

 __Examples__

 >>> _deleteWhere @BlogPost [[field| title |], [field| created_at |]]
 "DELETE FROM \"blogposts\" WHERE \"title\" = ? AND \"created_at\" = ?"

 @since 0.0.1.0
-}
_deleteWhere :: forall e. Entity e => Vector Field -> Query
_deleteWhere fs = textToQuery ("DELETE FROM " <> (getTableName @e)) <> _where fs

{-| Produce an ORDER BY clause with one field and a sorting keyword

 __Examples__

 >>> _orderBy ([field| title |], ASC)
 " ORDER BY \"title\" ASC"

 @since 0.0.2.0
-}
_orderBy :: (Field, SortKeyword) -> Query
_orderBy (f, sort) = textToQuery (" ORDER BY " <> quoteName (fieldName f) <> " " <> display sort)

{-| Produce an ORDER BY clause with many fields and sorting keywords

 __Examples__

 >>> _orderByMany (V.fromList [([field| title |], ASC), ([field| created_at |], DESC)])
 " ORDER BY \"title\" ASC, \"created_at\" DESC"

 @since 0.0.2.0
-}
_orderByMany :: Vector (Field, SortKeyword) -> Query
_orderByMany sortExpressions = textToQuery $ " ORDER BY " <> fold (intercalateVector ", " $ fmap renderSortExpression sortExpressions)
