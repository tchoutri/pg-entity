{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

-- |
--  Module      : Database.PostgreSQL.Entity.Internal.BlogPost
--  Copyright   : © Clément Delafargue, 2018
--                  Théophile Choutri, 2021
--                  Koz Ross, 2021
--  License     : MIT
--  Maintainer  : theophile@choutri.eu
--  Stability   : stable
--
--  Adapted from Clément Delafargue's [Yet Another Unsafe DB Layer](https://tech.fretlink.com/yet-another-unsafe-db-layer/)
--  article.
--
--  The models described in this module are used throughout the library's tests and docspecs.
module Database.PostgreSQL.Entity.Internal.BlogPost where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics (Generic)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))

import Data.ByteString.Builder (byteString, char8)
import qualified Data.List as List
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity (insert, insertMany)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (Entity (..), GenericEntity, PrimaryKey, TableName)

-- | Wrapper around the UUID type
newtype AuthorId = AuthorId {getAuthorId :: UUID}
  deriving
    (Eq, FromField, Ord, Show, ToField)
    via UUID

-- | Author data-type
data Author = Author
  { authorId :: AuthorId
  , name :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[PrimaryKey "author_id", TableName "authors"] Author)

instance HasField x Author a => IsLabel x (Author -> a) where
  fromLabel = getField @x

-- | Wrapper around the UUID type
newtype BlogPostId = BlogPostId {getBlogPostId :: UUID}
  deriving
    (Eq, FromField, Ord, Show, ToField)
    via UUID

newtype UUIDList = UUIDList {getUUIDList :: Vector UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, FromField, Ord)
    via Vector UUID

instance ToField UUIDList where
  toField (UUIDList vec) =
    if Vector.null vec
      then Plain (byteString "'{}'")
      else
        Many $
          Plain (byteString "ARRAY[")
            : (List.intersperse (Plain (char8 ',')) . fmap toField $ Vector.toList vec)
            ++ [Plain (char8 ']')]
            ++ [Plain (byteString " :: uuid[]")]

-- | The BlogPost data-type. Look at its 'Entity' instance declaration for how to handle
-- a "uuid[]" PostgreSQL type.
data BlogPost = BlogPost
  { blogPostId :: BlogPostId
  -- ^ Primary key
  , authorId :: AuthorId
  -- ^ Foreign keys, for which we need an explicit type annotation
  , uuidList :: UUIDList
  -- ^ A type that will need an explicit type annotation in the schema
  , title :: Text
  , content :: Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromRow, ToRow)

instance HasField x BlogPost a => IsLabel x (BlogPost -> a) where
  fromLabel = getField @x

instance Entity BlogPost where
  tableName = "blogposts"
  primaryKey = [field| blogpost_id |]
  fields =
    [ [field| blogpost_id |]
    , [field| author_id |]
    , [field| uuid_list |]
    , [field| title |]
    , [field| content |]
    , [field| created_at |]
    ]

-- | A specialisation of the 'Database.PostgreSQL.Entity.insert' function.
-- @insertBlogPost = insert \@BlogPost@
insertBlogPost :: BlogPost -> DBT IO ()
insertBlogPost = insert @BlogPost

-- | A function to insert many blogposts at once.
bulkInsertBlogPosts :: [BlogPost] -> DBT IO ()
bulkInsertBlogPosts = insertMany @BlogPost

-- | A specialisation of the 'Database.PostgreSQL.Entity.insert function.
-- @insertAuthor = insert \@Author@
insertAuthor :: Author -> DBT IO ()
insertAuthor = insert @Author

--

-- | A function to insert many authors at once.
bulkInsertAuthors :: [Author] -> DBT IO ()
bulkInsertAuthors = insertMany @Author

data Tags = Tags
  { category :: Text
  , labels :: [Text]
  }

instance Entity Tags where
  tableName = "tags"
  schema = Just "public"
  primaryKey = [field| category |]
  fields =
    [ [field| category |]
    , [field| labels |]
    ]
