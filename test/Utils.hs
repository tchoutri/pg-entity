{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Utils where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Kind
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import Data.Word
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection, SqlError (..))
import Database.PostgreSQL.Transact
import Env
import GHC.Generics
import Hedgehog (MonadGen (..))
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as Range
import Optics.Core
import Test.Tasty (TestTree)
import qualified Test.Tasty as Test
import qualified Test.Tasty.HUnit as Test

import Database.PostgreSQL.Entity.Internal.BlogPost
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Migration

newtype TestM (a :: Type) = TestM {getTestM :: ReaderT TestEnv IO a}
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadThrow)

data TestEnv = TestEnv
  { pool :: Pool Connection
  , connectionInfo :: ByteString
  }
  deriving stock (Generic)

data TestConfig = TestConfig
  { connectionInfo :: ByteString
  }
  deriving stock (Generic, Show, Eq)

liftDB :: DBT IO a -> TestM a
liftDB comp = do
  env <- getTestEnv
  let pool = env ^. #pool
  liftIO $
    catch
      (withPool pool comp)
      ( \(e :: SqlError) ->
          if sqlErrorMsg e == "connection disconnected"
            then withPool pool comp
            else throw e
      )

migrate :: Connection -> IO ()
migrate conn = void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./test/migrations"]

runTestM :: TestM a -> TestEnv -> IO a
runTestM comp env =
  runReaderT (getTestM comp) env

testThis :: String -> TestM () -> TestM TestTree
testThis name assertion = do
  env <- getTestEnv
  let test = runTestM assertion env
  pure $ Test.testCase name test

testThese :: String -> [TestM TestTree] -> TestM TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestM [TestTree]
    newTests = sequenceA tests

getTestEnv :: TestM TestEnv
getTestEnv = TestM ask

assertEqual :: (Eq a, Show a) => a -> a -> TestM ()
assertEqual expected actual = liftIO $ Test.assertEqual "" expected actual

--

genAuthorId :: MonadGen m => m AuthorId
genAuthorId = AuthorId <$> genUUID

genUUID :: MonadGen m => m UUID
genUUID = UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32
  where
    genWord32 :: MonadGen m => m Word32
    genWord32 = H.word32 (Range.constant minBound maxBound)

genUUIDList :: MonadGen m => m UUIDList
genUUIDList = UUIDList . V.fromList <$> H.list (Range.linear 1 10) genUUID

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  year <- toInteger <$> H.int (Range.constant 2000 2022)
  month <- H.int (Range.constant 1 12)
  day <- H.int (Range.constant 1 28)
  let date = fromGregorian year month day
  secs <- toInteger <$> H.int (Range.constant 0 86401)
  pure $ UTCTime date (secondsToDiffTime secs)

genName :: MonadGen m => m Text
genName = H.text (Range.constant 3 25) H.unicode

genAuthor :: MonadGen m => m Author
genAuthor = do
  authorId <- genAuthorId
  name <- genName
  createdAt <- genUTCTime
  pure Author{..}

data RandomAuthorTemplate m = RandomAuthorTemplate
  { generateAuthorId :: m AuthorId
  , generateName :: m Text
  , generateCreatedAt :: m UTCTime
  }
  deriving stock (Generic)

randomAuthorTemplate :: MonadIO m => RandomAuthorTemplate m
randomAuthorTemplate =
  RandomAuthorTemplate
    { generateAuthorId = H.sample genAuthorId
    , generateName = H.sample genName
    , generateCreatedAt = H.sample genUTCTime
    }

randomAuthor :: MonadIO m => RandomAuthorTemplate m -> m Author
randomAuthor RandomAuthorTemplate{..} = do
  authorId <- generateAuthorId
  name <- generateName
  createdAt <- generateCreatedAt
  pure Author{..}

instantiateRandomAuthor :: (MonadIO m, m ~ IO) => RandomAuthorTemplate m -> DBT m Author
instantiateRandomAuthor RandomAuthorTemplate{..} = do
  authorId <- liftIO generateAuthorId
  name <- liftIO generateName
  createdAt <- liftIO generateCreatedAt
  let author = Author{..}
  insertAuthor author
  pure author

--

genBlogPost :: MonadGen m => m BlogPost
genBlogPost = do
  blogPostId <- genBlogPostId
  authorId <- genAuthorId
  uuidList <- genUUIDList
  title <- genName
  content <- genName
  createdAt <- genUTCTime
  pure BlogPost{..}

genBlogPostId :: MonadGen m => m BlogPostId
genBlogPostId = BlogPostId <$> genUUID

data RandomBlogPostTemplate m = RandomBlogPostTemplate
  { generateBlogPostId :: m BlogPostId
  , generateAuthorId :: m AuthorId
  , generateUUIDList :: m UUIDList
  , generateTitle :: m Text
  , generateContent :: m Text
  , generateCreatedAt :: m UTCTime
  }
  deriving stock (Generic)

randomBlogPostTemplate :: MonadIO m => RandomBlogPostTemplate m
randomBlogPostTemplate =
  RandomBlogPostTemplate
    { generateBlogPostId = H.sample genBlogPostId
    , generateAuthorId = H.sample genAuthorId
    , generateUUIDList = H.sample genUUIDList
    , generateTitle = H.sample genName
    , generateContent = H.sample genName
    , generateCreatedAt = H.sample genUTCTime
    }

randomBlogPost :: MonadIO m => RandomBlogPostTemplate m -> m BlogPost
randomBlogPost RandomBlogPostTemplate{..} = do
  blogPostId <- generateBlogPostId
  authorId <- generateAuthorId
  uuidList <- generateUUIDList
  title <- generateTitle
  content <- generateContent
  createdAt <- generateCreatedAt
  pure BlogPost{..}

instantiateRandomBlogPost :: (MonadIO m, m ~ IO) => RandomBlogPostTemplate m -> DBT m BlogPost
instantiateRandomBlogPost RandomBlogPostTemplate{..} = do
  blogPostId <- liftIO generateBlogPostId
  authorId <- liftIO generateAuthorId
  uuidList <- liftIO generateUUIDList
  title <- liftIO generateTitle
  content <- liftIO generateContent
  createdAt <- liftIO generateCreatedAt
  let blogPost = BlogPost{..}
  insertBlogPost blogPost
  pure blogPost

retrieveTestEnv :: IO TestConfig
retrieveTestEnv =
  Env.parse id parseTestConfig

parseConnectionInfo :: Parser Error ByteString
parseConnectionInfo =
  var str "DB_CONNSTRING" (help "libpq-compatible connection string")

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parseConnectionInfo

testConfigToTestEnv :: TestConfig -> IO TestEnv
testConfigToTestEnv TestConfig{..} = do
  let connectionTimeout = 100
  let maxResources = 10
  pool <- mkPool connectionInfo connectionTimeout maxResources
  pure TestEnv{..}

mkPool
  :: ByteString -- Database access information
  -> NominalDiffTime -- Allowed timeout
  -> Int -- Number of connections
  -> IO (Pool PG.Connection)
mkPool connectionInfo timeout' maxResources =
  Pool.newPool $
    Pool.setNumStripes (Just 10) $
      Pool.defaultPoolConfig
        (PG.connectPostgreSQL connectionInfo)
        PG.close
        (realToFrac timeout')
        maxResources
