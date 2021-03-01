{-# LANGUAGE OverloadedLists #-}
module EntitySpec where

import qualified Data.Vector as V
-- import Database.PostgreSQL.Simple (Connection, Only (..))
import Database.PostgreSQL.Simple 
import Database.PostgreSQL.Simple.Migration 
import Relude.Unsafe (read)
import Test.Hspec 
import Test.Hspec.DB (describeDB, itDB)

-- import Database.PostgreSQL.Entity (selectById, selectOneByField, selectManyByField, deleteByField, delete)
import Database.PostgreSQL.Entity 
import Database.PostgreSQL.Entity.BlogPost

blogPost1 :: BlogPost
blogPost1 =
  let blogPostId = BlogPostId (read "55b3821a-79e4-11eb-ad18-5405db82c3cd")
      authorIds  = [AuthorId (read "74c3fa0e-79e4-11eb-8792-5405db82c3cd")]
      title      = "Echoes from the other world"
      content    = "Send out a sound\nFor the wood between the worlds\nGently repeat\nAs the boundaries start to swirl"
      createdAt  = read "2021-02-28 17:48:15 UTC"
  in BlogPost{..}

blogPost2 :: BlogPost
blogPost2 =
  let blogPostId = BlogPostId (read "a8df9980-79fb-11eb-a061-5405db82c3cd")
      authorIds  = [AuthorId (read "ae9800e2-79fb-11eb-a542-5405db82c3cd")]
      title      = "A Past and Future Secret"
      content    = "Oh, I haven't been here for a while\nIn blindness and decay\nThe circle's been closed, now"
      createdAt  = read "2021-02-28 20:33:25 UTC"
  in BlogPost{..}

blogPost3 :: BlogPost
blogPost3 =
  let blogPostId = BlogPostId (read "1a032938-79fc-11eb-b1ec-5405db82c3cd")
      authorIds  = [AuthorId (read "ae9800e2-79fb-11eb-a542-5405db82c3cd")]
      title      = "The Script for my requiem"
      content    = "[…]"
      createdAt  = read "2021-02-28 21:23:25 UTC"
  in BlogPost{..}

blogPost4 :: BlogPost
blogPost4 =
  let blogPostId = BlogPostId (read "1e1e6dac-79fc-11eb-b6a6-5405db82c3cd")
      authorIds  = [AuthorId (read "ae9800e2-79fb-11eb-a542-5405db82c3cd")]
      title      = "Mordred's Song"
      content    = "[…]"
      createdAt  = read "2021-02-28 21:24:35 UTC"
  in BlogPost{..}


migrate :: Connection -> IO ()
migrate conn = void $ runMigrations False conn [MigrationInitialization, MigrationDirectory "./test/migrations"]

spec :: Spec
spec = describeDB migrate "Entity DB " $ do
  itDB "Insert blog posts" $ do
    insertBlogPost blogPost1 
    insertBlogPost blogPost2
    insertBlogPost blogPost3
    insertBlogPost blogPost4
    result <- selectById (blogPostId blogPost1)
    pure $ result `shouldBe` blogPost1
  itDB "Select blog post by title" $ do
    result <- selectOneByField @BlogPost "title" ("A Past and Future Secret" :: Text)
    pure $ result `shouldBe` blogPost2
  itDB "Select multiple blog posts by author id" $ do
    result <- selectManyByField @BlogPost "author_ids" (authorIds blogPost4)
    pure $ V.toList result `shouldMatchList` [blogPost4, blogPost3]
  itDB "Delete a blog post" $ do
    delete @BlogPost (Only (blogPostId blogPost2))
    result <- selectManyByField @BlogPost "blogpost_id" (blogPostId blogPost2)
    pure $ V.length result `shouldBe` 0
  itDB "Delete a blog post by title" $ do
    deleteByField @BlogPost ["title"] (Only @Text "Echoes from the other world")
    result <- selectManyByField @BlogPost "title" (title blogPost1)
    pure $ V.length result `shouldBe` 0
