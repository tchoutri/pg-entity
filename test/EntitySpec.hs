{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module EntitySpec where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Database.PostgreSQL.Entity
  ( delete
  , deleteByField
  , joinSelectOneByField
  , selectById
  , selectManyByField
  , selectOneByField
  , selectOneWhereIn
  , selectOrderBy
  , selectWhereNotNull
  , selectWhereNull
  , update
  , updateFieldsBy
  , _joinSelectWithFields
  , _where
  )
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Internal.BlogPost
  ( Author (..)
  , AuthorId (..)
  , BlogPost (..)
  , bulkInsertAuthors
  , bulkInsertBlogPosts
  , insertAuthor
  , insertBlogPost
  , upsertBlogPost
  )
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Transact (DBT)

import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Set as Set
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types
import Optics.Core
import Test.Tasty
import Utils
import qualified Utils as U

spec :: TestM TestTree
spec =
  testThese
    "Entity Tests"
    [ testThis "Select blog post by title" selectBlogPostByTitle
    , testThis "Select blog posts by null and non-null condition" selectByNullAndNonNull
    , testThis "Select multiple blog posts by author id" selectManyByAuthorId
    , testThis "Delete blog posts" deleteBlogPosts
    , testThis "Get all the article titles by author name" getAllTitlesByAuthorName
    , testThis "Change the name of an author" changeAuthorName
    , testThis "Select a row when the value of title is in an array of possible values" selectWhereIn
    , testThis "SELECT ORDER BY yields the appropriate results" testSelectOrderBy
    , testThis "select blog posts by author's name" selectBlogpostsByAuthorName
    , testThis "Insert many blog posts" insertManyBlogPosts
    , testThis "Upsert a blog post" testUpsertBlogPost
    ]

selectBlogPostByTitle :: TestM ()
selectBlogPostByTitle = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate
  blogPost <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  result <- liftDB $ selectOneByField @BlogPost [field| title |] (Only (blogPost ^. #title))
  U.assertEqual result (Just blogPost)

selectByNullAndNonNull :: TestM ()
selectByNullAndNonNull = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate

  liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}

  result <- liftDB $ selectWhereNotNull @BlogPost [[field| author_id |], [field| title |]]
  U.assertEqual True (not . null $ result)
  result1 <- liftDB $ selectWhereNull @BlogPost [[field| author_id |], [field| content |]]
  U.assertEqual 0 (V.length result1)

selectManyByAuthorId :: TestM ()
selectManyByAuthorId = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate

  blogPost4 <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  blogPost2 <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  blogPost3 <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  result <- liftDB $ selectManyByField @BlogPost [field| author_id |] $ Only (#authorId blogPost4)
  U.assertEqual (Set.fromList [blogPost2, blogPost3, blogPost4]) (Set.fromList $ V.toList result)

deleteBlogPosts :: TestM ()
deleteBlogPosts = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate

  blogPost1 <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateTitle = pure "Echoes from the other world", generateAuthorId = pure (author ^. #authorId)}
  blogPost2 <- liftDB $ instantiateRandomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  liftDB $ delete @BlogPost (Only (blogPostId blogPost2))
  result <- liftDB $ selectManyByField @BlogPost [field| blogpost_id |] $ Only (#blogPostId blogPost2)
  U.assertEqual 0 (V.length result)

  liftDB $ deleteByField @BlogPost [[field| title |]] (Only @Text "Echoes from the other world")
  result1 <- liftDB $ selectManyByField @BlogPost [field| title |] $ Only (#title blogPost1)
  U.assertEqual 0 (V.length result1)

getAllTitlesByAuthorName :: TestM ()
getAllTitlesByAuthorName = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate{generateName = pure "Hansi Kürsch"}

  liftDB $
    instantiateRandomBlogPost
      randomBlogPostTemplate
        { generateTitle = pure "The Script for my requiem"
        , generateAuthorId = pure (author ^. #authorId)
        }
  liftDB $
    instantiateRandomBlogPost
      randomBlogPostTemplate
        { generateAuthorId = pure (author ^. #authorId)
        , generateTitle = pure "Mordred's Song"
        }

  let q =
        _joinSelectWithFields @BlogPost @Author [[field| title |]] [[field| name |]]
          <> _where [[field| name |]]
  result <- liftDB (query Select q (Only ("Hansi Kürsch" :: Text)) :: (MonadIO m) => DBT m (Vector (Text, Text)))
  U.assertEqual [("The Script for my requiem", "Hansi Kürsch"), ("Mordred's Song", "Hansi Kürsch")] result

changeAuthorName :: TestM ()
changeAuthorName = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate

  blogPost <- randomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  liftDB $ insertBlogPost blogPost

  let newAuthor = author{name = "Hannah Kürsch"}
  liftDB $ update @Author newAuthor
  result1 <- liftDB $ selectById (Only (#authorId author))
  U.assertEqual (Just newAuthor) result1

  author2 <- liftDB $ instantiateRandomAuthor randomAuthorTemplate
  let newAuthorId = UUID.toText $ getAuthorId $ #authorId author2 :: Text
  let newTitle = "Something Entirely New with a lone quote '" :: Text
  modifiedRows <- liftDB $ updateFieldsBy @BlogPost [[field| author_id |], [field| title |]] ([field| title |], #title blogPost) (newAuthorId, newTitle)
  result2 <- liftDB $ selectManyByField @BlogPost [field| author_id |] (Only (#authorId author2))
  U.assertEqual (fromIntegral modifiedRows) (V.length result2)

  let oldName = "Johnson McElroy" :: Text
  liftDB $ instantiateRandomAuthor randomAuthorTemplate{generateName = pure oldName}
  let newName = "Tiberus McElroy" :: Text
  result3 <- liftDB $ updateFieldsBy @Author [[field| name |]] ([field| name |], oldName) (Only newName)
  U.assertEqual 1 result3

selectWhereIn :: TestM ()
selectWhereIn = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate
  blogPost <-
    liftDB $
      instantiateRandomBlogPost
        randomBlogPostTemplate
          { generateAuthorId = pure (author ^. #authorId)
          , generateTitle = pure "Testing unescaped single quotes ' :)"
          }
  result <- liftDB $ selectOneWhereIn @BlogPost [field| title |] ["Testing unescaped single quotes ' :)", "Doesn't exist lol"]
  U.assertEqual (Just blogPost) result

testSelectOrderBy :: TestM ()
testSelectOrderBy = do
  author1 <-
    liftDB $
      instantiateRandomAuthor
        randomAuthorTemplate
          { generateName = pure "Alphabetically first"
          , generateCreatedAt = pure (read "2013-03-16 21:38:36Z")
          }

  author2 <-
    liftDB $
      instantiateRandomAuthor
        randomAuthorTemplate
          { generateName = pure "Blphabetically first"
          , generateCreatedAt = pure (read "2012-03-16 21:38:36Z")
          }

  let authors = V.fromList [author1, author2]

  result1 <- V.filter (\a -> a `V.elem` authors) <$> liftDB (selectOrderBy @Author (V.fromList [([field| name |], ASC)]))
  U.assertEqual authors result1

  let reverseAuthors = V.fromList [author2, author1]
  result2 <- V.filter (\a -> a `V.elem` authors) <$> liftDB (selectOrderBy @Author (V.fromList [([field| name |], DESC)]))
  U.assertEqual reverseAuthors result2

  author3 <-
    liftDB $
      instantiateRandomAuthor
        randomAuthorTemplate
          { generateName = pure "Blphabetically first"
          , generateCreatedAt = pure (read "2011-03-16 21:38:36Z")
          }
  let threeAuthors = V.fromList [author1, author3, author2]
  result3 <-
    V.filter (\a -> a `V.elem` threeAuthors)
      <$> liftDB (selectOrderBy @Author (V.fromList [([field| name |], ASC), ([field| created_at |], ASC)]))
  U.assertEqual threeAuthors result3

selectBlogpostsByAuthorName :: TestM ()
selectBlogpostsByAuthorName = do
  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate{generateName = pure "Alfonso Bertholt"}
  blogPost1 <-
    liftDB $
      instantiateRandomBlogPost
        randomBlogPostTemplate
          { generateAuthorId = pure (author ^. #authorId)
          }
  blogPost2 <-
    liftDB $
      instantiateRandomBlogPost
        randomBlogPostTemplate
          { generateAuthorId = pure (author ^. #authorId)
          }
  result <- liftDB $ joinSelectOneByField @BlogPost @Author [field| author_id |] [field| name |] (author ^. #name)
  U.assertEqual (S.fromList [blogPost1, blogPost2]) (S.fromList $ V.toList result)

insertManyBlogPosts :: TestM ()
insertManyBlogPosts = do
  author1 <- randomAuthor randomAuthorTemplate{generateName = pure "Vivienne Brooks"}
  author2 <- randomAuthor randomAuthorTemplate{generateName = pure "Léana Garibaldi"}
  void $ liftDB $ bulkInsertAuthors [author1, author2]

  author <- liftDB $ instantiateRandomAuthor randomAuthorTemplate{generateName = pure "Léana Garibaldi"}
  blogPost1 <- randomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  blogPost2 <- randomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author ^. #authorId)}
  void $ liftDB $ bulkInsertBlogPosts [blogPost1, blogPost2]
  result <- liftDB $ joinSelectOneByField @BlogPost @Author [field| author_id |] [field| name |] (author ^. #name)
  U.assertEqual (S.fromList [blogPost1, blogPost2]) (S.fromList $ V.toList result)

testUpsertBlogPost :: TestM ()
testUpsertBlogPost = do
  author1 <- randomAuthor randomAuthorTemplate{generateName = pure "Vivienne Brooks"}
  void $ liftDB $ insertAuthor author1
  blogPost1 <- randomBlogPost randomBlogPostTemplate{generateAuthorId = pure (author1 ^. #authorId)}
  blogPost2 <- randomBlogPost randomBlogPostTemplate{generateBlogPostId = pure (blogPost1 ^. #blogPostId), generateAuthorId = pure (author1 ^. #authorId), generateTitle = pure "New title"}

  void $ liftDB $ insertBlogPost blogPost1
  void $ liftDB $ upsertBlogPost blogPost2 [[field| title |]]

  r <- liftDB $ selectById @BlogPost (Only (blogPost1 ^. #blogPostId))
  let result = fromJust r
  U.assertEqual (result ^. #title) (blogPost2 ^. #title)
