{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

module EntitySpec where

import qualified Data.Vector as V
import Database.PostgreSQL.Entity.DBT.Types (QueryNature (Select))
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization),
                                             runMigrations)
import Database.PostgreSQL.Transact (DBT)
import Relude.Unsafe (read)
import Test.Hspec (Spec, shouldBe, shouldMatchList)
import Test.Hspec.DB (describeDB, itDB)

import Database.PostgreSQL.Entity (_crossSelectWithFields, delete, deleteByField, selectById, selectManyByField,
                                   selectOneByField, selectWhereNotNull, selectWhereNull)
import Database.PostgreSQL.Entity.BlogPost (Author (..), AuthorId (AuthorId), BlogPost (..), BlogPostId (BlogPostId),
                                            insertAuthor, insertBlogPost)
import Database.PostgreSQL.Entity.DBT (query_)

author1 :: Author
author1 =
  let authorId = AuthorId (read "74c3fa0e-79e4-11eb-8792-5405db82c3cd")
      name = "Jamie Reynolds"
      createdAt = read "2021-02-28 17:30:13 UTC"
   in Author{..}

author2 :: Author
author2 =
  let authorId = AuthorId (read "bc97c16c-7b83-11eb-83e5-5405db82c3cd")
      name = "Hansi Kürsch"
      createdAt = read "2021-02-28 17:30:17 UTC"
   in Author{..}

blogPost1 :: BlogPost
blogPost1 =
  let blogPostId = BlogPostId (read "55b3821a-79e4-11eb-ad18-5405db82c3cd")
      authorId   = #authorId author1
      uuidList   = [read "4401d848-7b40-11eb-b148-5405db82c3cd", read "4bbf0786-7b40-11eb-b1d9-5405db82c3cd"]
      title      = "Echoes from the other world"
      content    = "Send out a sound\nFor the wood between the worlds\nGently repeat\nAs the boundaries start to swirl"
      createdAt  = read "2021-02-28 17:48:15 UTC"
  in BlogPost{..}

blogPost2 :: BlogPost
blogPost2 =
  let blogPostId = BlogPostId (read "a8df9980-79fb-11eb-a061-5405db82c3cd")
      authorId   = #authorId author2
      uuidList   = [read "4401d848-7b40-11eb-b148-5405db82c3cd", read "4bbf0786-7b40-11eb-b1d9-5405db82c3cd"]
      title      = "A Past and Future Secret"
      content    = "Oh, I haven't been here for a while\nIn blindness and decay\nThe circle's been closed, now"
      createdAt  = read "2021-02-28 20:33:25 UTC"
  in BlogPost{..}

blogPost3 :: BlogPost
blogPost3 =
  let blogPostId = BlogPostId (read "1a032938-79fc-11eb-b1ec-5405db82c3cd")
      authorId   = #authorId author2
      uuidList   = [read "4401d848-7b40-11eb-b148-5405db82c3cd", read "4bbf0786-7b40-11eb-b1d9-5405db82c3cd"]
      title      = "The Script for my requiem"
      content    = "[…]"
      createdAt  = read "2021-02-28 21:23:25 UTC"
  in BlogPost{..}

blogPost4 :: BlogPost
blogPost4 =
  let blogPostId = BlogPostId (read "1e1e6dac-79fc-11eb-b6a6-5405db82c3cd")
      authorId   = #authorId author2
      uuidList   = [read "4401d848-7b40-11eb-b148-5405db82c3cd", read "4bbf0786-7b40-11eb-b1d9-5405db82c3cd"]
      title      = "Mordred's Song"
      content    = "[…]"
      createdAt  = read "2021-02-28 21:24:35 UTC"
  in BlogPost{..}


migrate :: Connection -> IO ()
migrate conn = void $ runMigrations False conn [MigrationInitialization, MigrationDirectory "./test/migrations"]

spec :: Spec
spec = describeDB migrate "Entity DB " $ do
  itDB "Insert authors" $ do
    insertAuthor author1
    insertAuthor author2
  itDB "Insert blog posts" $ do
    insertBlogPost blogPost1
    insertBlogPost blogPost2
    insertBlogPost blogPost3
    insertBlogPost blogPost4
    result <- selectById $ Only (#blogPostId blogPost1)
    pure $ result `shouldBe` blogPost1
  itDB "Select blog post by title" $ do
    result <- selectOneByField @BlogPost "title" $ Only ("A Past and Future Secret" :: Text)
    pure $ result `shouldBe` blogPost2
  itDB "Select all blog posts by non-null condition" $ do
    result <- selectWhereNotNull @BlogPost ["author_id", "title"]
    pure $ V.toList result `shouldMatchList` [blogPost1, blogPost2, blogPost3, blogPost4]
  itDB "Select no blog post by null condition" $ do
    result <- selectWhereNull @BlogPost ["author_id", "content"]
    pure $ V.length result `shouldBe` 0
  itDB "Select multiple blog posts by author id" $ do
    result <- selectManyByField @BlogPost "author_id" $ Only (#authorId blogPost4)
    pure $ V.toList result `shouldMatchList` [blogPost4, blogPost3]
  itDB "Delete a blog post" $ do
    delete @BlogPost (Only (blogPostId blogPost2))
    result <- selectManyByField @BlogPost "blogpost_id" $ Only (#blogPostId blogPost2)
    pure $ V.length result `shouldBe` 0
  itDB "Delete a blog post by title" $ do
    deleteByField @BlogPost ["title"] (Only @Text "Echoes from the other world")
    result <- selectManyByField @BlogPost "title" $ Only (#title blogPost1)
    pure $ V.length result `shouldBe` 0
  itDB "Get all the article titles by author name" $ do
    let q = _crossSelectWithFields @BlogPost @Author ["title"] ["name"]
    result <- query_ Select q :: (MonadIO m) => DBT m (V.Vector (Text, Text))
    pure $ result `shouldBe` [("The Script for my requiem","Hansi Kürsch"),("Mordred's Song","Hansi Kürsch")]

