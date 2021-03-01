{-# LANGUAGE StrictData #-}
{-|
  Module      : Database.PostgreSQL.Entity.BlogPost
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : stable

  Directly taken from Clément Delafargue's [Yet Another Unsafe DB Layer](https://tech.fretlink.com/yet-another-unsafe-db-layer/)
  article. The model is used throughout the library's tests and docspecs.
-}
module Database.PostgreSQL.Entity.BlogPost where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Entity (Entity (..), insert, withType)
import Database.PostgreSQL.Entity.DBT.Types (DBT, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

newtype BlogPostId
  = BlogPostId { getBlogPostId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)
newtype AuthorId
  = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, FromField, Show, ToField)

data BlogPost
  = BlogPost { blogPostId :: BlogPostId
               -- ^ Primary key
             , authorIds  :: Vector AuthorId
               -- ^ Foreign keys, for which we need an explicit type annotation
             , title      :: Text
             , content    :: Text
             , createdAt  :: UTCTime
             }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)

instance Entity BlogPost where
  tableName  = "blogposts"
  primaryKey = "blogpost_id"
  fields = V.fromList [ "blogpost_id"
                      , "author_ids" `withType` "uuid[]"
                      , "title"
                      , "content"
                      , "created_at"
                      ]

-- | A specialisation of the 'Database.PostgreSQL.Entity.insert' function.
insertBlogPost :: BlogPost -> DBT IO ()
insertBlogPost = insert @BlogPost
