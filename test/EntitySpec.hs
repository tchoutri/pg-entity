{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module EntitySpec where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.PostgreSQL.Entity.DBT.Types (QueryNature (Select))
import Database.PostgreSQL.Entity.QQ (field)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization),
                                             runMigrations)
import Database.PostgreSQL.Transact (DBT)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldMatchList, shouldReturn)

import Database.PostgreSQL.Entity (_joinSelectWithFields, delete, deleteByField, selectById, selectManyByField,
                                   selectOneByField, selectWhereNotNull, selectWhereNull, update, updateFieldsBy)
import Database.PostgreSQL.Entity.DBT (query_)
import Database.PostgreSQL.Entity.Internal.BlogPost (Author (..), AuthorId (..), BlogPost (..), BlogPostId (BlogPostId),
                                                     insertAuthor, insertBlogPost)

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

author3 :: Author
author3 =
  let authorId = AuthorId (read "3aa9232e-7f6d-11eb-823f-5405db82c3cd")
      name = "Johnson McElroy"
      createdAt = read "2021-02-28 17:31:39 UTC"
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
    insertAuthor author3
  itDB "Insert blog posts" $ do
    insertBlogPost blogPost1
    insertBlogPost blogPost2
    insertBlogPost blogPost3
    insertBlogPost blogPost4
    result <- selectById $ Only (#blogPostId blogPost1)
    result `shouldBe` Just blogPost1
  itDB "Select blog post by title" $ do
    selectOneByField @BlogPost [field| title |] (Only ("A Past and Future Secret" :: Text))
      `shouldReturn` Just blogPost2
  itDB "Select all blog posts by non-null condition" $ do
    result <- selectWhereNotNull @BlogPost [[field| author_id |], [field| title |]]
    V.toList result `shouldMatchList` [blogPost1, blogPost2, blogPost3, blogPost4]
  itDB "Select no blog post by null condition" $ do
    result <- selectWhereNull @BlogPost [[field| author_id |], [field| content |]]
    V.length result `shouldBe` 0
  itDB "Select multiple blog posts by author id" $ do
    result <- selectManyByField @BlogPost [field| author_id |] $ Only (#authorId blogPost4)
    V.toList result `shouldMatchList` [blogPost2, blogPost3, blogPost4]
  itDB "Delete a blog post" $ do
    delete @BlogPost (Only (blogPostId blogPost2))
    result <- selectManyByField @BlogPost [field| blogpost_id |] $ Only (#blogPostId blogPost2)
    V.length result `shouldBe` 0
  itDB "Delete a blog post by title" $ do
    deleteByField @BlogPost [[field| title |]] (Only @Text "Echoes from the other world")
    result <- selectManyByField @BlogPost [field| title |] $ Only (#title blogPost1)
    V.length result `shouldBe` 0
  itDB "Get all the article titles by author name" $ do
    let q = _joinSelectWithFields @BlogPost @Author [[field| title |]] [[field| name |]]
    (query_ Select q :: (MonadIO m) => DBT m (V.Vector (Text, Text)))
      `shouldReturn` [("The Script for my requiem","Hansi Kürsch"),("Mordred's Song","Hansi Kürsch")]
  itDB "Change the name of an author" $ do
    let newAuthor = author2{name = "Hannah Kürsch"}
    update @Author newAuthor
    selectById (Only (#authorId author2))
      `shouldReturn` Just newAuthor
  itDB "Change the name of an author according to their name" $ do
    let newName = "Tiberus McElroy" :: Text
    let oldName = "Johnson McElroy" :: Text
    updateFieldsBy @Author [[field| name |]] ([field| name |], oldName) (Only newName)
     `shouldReturn` 1
  itDB "Change the author and title of a blogpost" $ do
    let newAuthorId =  UUID.toText $ getAuthorId $ #authorId author3 :: Text
    let newTitle    = "Something Entirely New" :: Text
    modifiedRows <- updateFieldsBy @BlogPost [[field| author_id |], [field| title |]] ([field| title |], #title blogPost4) (newAuthorId, newTitle)
    result <- selectManyByField @BlogPost [field| author_id |] (Only (#authorId author3))
    V.length result `shouldBe` fromIntegral modifiedRows
