{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module GenericsSpec where

import BlogPost
import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID
import Data.Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Internal (getTableName)
import Database.PostgreSQL.Entity.Internal.Unsafe (Field (Field))
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import GHC.Generics
import Test.Tasty
import Utils
import qualified Utils as U

data TestType = Test
  { fieldOne :: Int
  , fieldTwo :: Text
  , fieldThree :: [Int]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Entity)

data Apple = AppleCons
  { thisField :: Text
  , thatField :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving
    (Entity)
    via (GenericEntity '[TableName "apples"] Apple)

data Project = Project
  { createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , id :: Int
  , title :: Text
  , description :: Text
  , hosts :: Vector Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)

newtype ProjectId
  = ProjectId UUID
  deriving
    (Eq, FromField, Show, ToField)
    via UUID

newtype EndpointId
  = EndpointId UUID
  deriving
    (Eq, FromField, Show, ToField)
    via UUID

data Endpoint = Endpoint
  { enpcreatedAt :: ZonedTime
  , enpupdatedAt :: ZonedTime
  , enpprojectId :: ProjectId
  , enpid :: EndpointId
  , enpurlPath :: Text
  , enpurlParams :: Value
  , enpmethod :: Text
  , enphosts :: Vector Text
  , enprequestHashes :: Vector Text
  , enpresponseHashes :: Vector Text
  , enpqueryparamHashes :: Vector Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "endpoints", Schema "apis", PrimaryKey "id", FieldModifiers '[StripPrefix "enp", CamelToSnake]] Endpoint)

endpointsByProject :: ProjectId -> DBT IO (Vector Endpoint)
endpointsByProject pid = selectManyByField @Endpoint [field| project_id |] (Only pid)

endpointById :: EndpointId -> DBT IO (Maybe Endpoint)
endpointById eId = selectById @Endpoint (Only eId)

spec :: TestM TestTree
spec =
  testThese
    "Generic deriving tests"
    [ testThis "TestType has the expected table name" testExpectedTableName
    , testThis "TestType has the expected field list" testExpectedFieldList
    , testThis "TestType has the expected primary key" testExpectedPrimaryKey
    , testThis "Apple has the expected primary key" testInferredPrimaryKey
    , testThis "Apple has the expected table name" testSpecifiedTableName
    , testThis "Apple has the expected fields" testInferredFields
    , testThis "Prefix stripping works" testPrefixStrippingWorks
    , testThis "Explicit schema works" testExplicitSchemaWorks
    , testThis "Generically derived schema works" testGenericallyDerivedSchema
    , testThis "Derived primary key is not necessarily the first field" testDerivePrimaryKey
    ]

testExpectedTableName :: TestM ()
testExpectedTableName =
  U.assertEqual "test_type" (tableName @TestType)

testExpectedFieldList :: TestM ()
testExpectedFieldList =
  U.assertEqual [[field| field_one |], [field| field_two |], [field| field_three |]] (fields @TestType)

testExpectedPrimaryKey :: TestM ()
testExpectedPrimaryKey =
  U.assertEqual (Field "field_one" Nothing) (primaryKey @TestType)

testInferredPrimaryKey :: TestM ()
testInferredPrimaryKey =
  U.assertEqual (Field "this_field" Nothing) (primaryKey @Apple)

testSpecifiedTableName :: TestM ()
testSpecifiedTableName =
  U.assertEqual "apples" (tableName @Apple)

testInferredFields :: TestM ()
testInferredFields =
  U.assertEqual [[field| this_field |], [field| that_field |]] (fields @Apple)

testPrefixStrippingWorks :: TestM ()
testPrefixStrippingWorks =
  U.assertEqual (fields @Endpoint) [[field| created_at |], [field| updated_at |], [field| project_id |], [field| id |], [field| url_path |], [field| url_params |], [field| method |], [field| hosts |], [field| request_hashes |], [field| response_hashes |], [field| queryparam_hashes |]]

testExplicitSchemaWorks :: TestM ()
testExplicitSchemaWorks =
  U.assertEqual "public.\"tags\"" (getTableName @Tags)

testGenericallyDerivedSchema :: TestM ()
testGenericallyDerivedSchema =
  U.assertEqual "apis.\"endpoints\"" (getTableName @Endpoint)

testDerivePrimaryKey :: TestM ()
testDerivePrimaryKey = do
  U.assertEqual [field| id |] (primaryKey @Endpoint)
