{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-|
  Module      : Database.PostgreSQL.Entity.BlogPost
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Adapted from Clément Delafargue's [Yet Another Unsafe DB Layer](https://tech.fretlink.com/yet-another-unsafe-db-layer/)
  article.

  The models described in this module are used throughout the library's tests and docspecs.
-}
module Database.PostgreSQL.Entity.BlogPost where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Transact (DBT)
import GHC.Records (HasField (..))

import Database.PostgreSQL.Entity (Entity (..), insert, withType)

newtype AuthorId
  = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)

data Author
  = Author { authorId  :: AuthorId
           , name      :: Text
           , createdAt :: UTCTime
           }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

instance HasField x Author a => IsLabel x (Author -> a) where
  fromLabel = getField @x

instance Entity Author where
  tableName  = "authors"
  primaryKey = "author_id"
  fields     = [ "author_id"
               , "name"
               , "created_at"
               ]

newtype BlogPostId
  = BlogPostId { getBlogPostId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)
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
insertBlogPost :: BlogPost -> DBT IO ()
insertBlogPost = insert @BlogPost

insertAuthor :: Author -> DBT IO ()
insertAuthor = insert @Author
