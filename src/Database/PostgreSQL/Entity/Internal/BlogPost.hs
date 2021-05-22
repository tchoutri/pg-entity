{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-|
  Module      : Database.PostgreSQL.Entity.Internal.BlogPost
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Adapted from Clément Delafargue's [Yet Another Unsafe DB Layer](https://tech.fretlink.com/yet-another-unsafe-db-layer/)
  article.

  The models described in this module are used throughout the library's tests and docspecs.
-}
module Database.PostgreSQL.Entity.Internal.BlogPost where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Transact (DBT)
import GHC.Records (HasField (..))

import Database.PostgreSQL.Entity (insert, withType)
import Database.PostgreSQL.Entity.Types

-- | Wrapper around the UUID type
newtype AuthorId
  = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)

-- | Author data-type
data Author
  = Author { authorId  :: AuthorId
           , name      :: Text
           , createdAt :: UTCTime
           }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity) via (GenericEntity '[PrimaryKey "author_id", TableName "authors"] Author)

instance HasField x Author a => IsLabel x (Author -> a) where
  fromLabel = getField @x

-- instance Entity Author where
--   tableName  = "authors"
--   primaryKey = "author_id"
--   fields     = [ "author_id"
--                , "name"
--                , "created_at"
--                ]

-- | Wrapper around the UUID type
newtype BlogPostId
  = BlogPostId { getBlogPostId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)

-- | The BlogPost data-type. Look at its 'Entity' instance declaration for how to handle
-- a "uuid[]" PostgreSQL type.
data BlogPost
  = BlogPost { blogPostId :: BlogPostId
               -- ^ Primary key
             , authorId   :: AuthorId
               -- ^ Foreign keys, for which we need an explicit type annotation
             , uuidList   :: Vector UUID
               -- ^ A type that will need an explicit type annotation in the schema
             , title      :: Text
             , content    :: Text
             , createdAt  :: UTCTime
             }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

instance HasField x BlogPost a => IsLabel x (BlogPost -> a) where
  fromLabel = getField @x

instance Entity BlogPost where
  tableName  = "blogposts"
  primaryKey = "blogpost_id"
  fields = [ "blogpost_id"
           , "author_id"
           , "uuid_list" `withType` "uuid[]"
           , "title"
           , "content"
           , "created_at"
           ]

-- | A specialisation of the 'Database.PostgreSQL.Entity.insert' function.
-- insertBlogPost = insert @BlogPost
insertBlogPost :: BlogPost -> DBT IO ()
insertBlogPost = insert @BlogPost

-- | A specialisation of the 'Database.PostgreSQL.Entity.insert function.
-- insertAuthor = insert @Author
insertAuthor :: Author -> DBT IO ()
insertAuthor = insert @Author
