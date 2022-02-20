{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module GenericsSpec where

import Data.Text
import Data.UUID
import Data.Vector
import Database.PostgreSQL.Entity.Internal (getTableName)
import Database.PostgreSQL.Entity.Internal.BlogPost
import Database.PostgreSQL.Entity.Internal.Unsafe (Field (Field))
import Database.PostgreSQL.Entity.Types
import GHC.Generics
import Test.Tasty
import qualified Utils as U
import Utils

data TestType
  = Test { fieldOne   :: Int
         , fieldTwo   :: Text
         , fieldThree :: [Int]
         }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Entity)

data Apple
  = AppleCons { thisField :: Text
              , thatField :: Text
              }
  deriving stock (Eq, Generic, Show)
  deriving (Entity)
    via (GenericEntity '[TableName "apples"] Apple)

data Endpoint
  = Endpoint { enpID            :: UUID
             , enpProjectId     :: UUID
             , enpRequestHashes :: Vector Text
             }
  deriving (Generic, Show)
  deriving (Entity)
    via (GenericEntity '[TableName "endpoints", Schema "apis", PrimaryKey "id", FieldModifiers '[StripPrefix "enp", CamelToSnake]] Endpoint)

spec :: TestM TestTree
spec = testThese "Generic deriving tests"
  [ testThis "TestType has the expected table name" testExpectedTableName
  , testThis "TestType has the expected field list" testExpectedFieldList
  , testThis "TestType has the expected primary key" testExpectedPrimaryKey
  , testThis "Apple has the expected primary key" testInferredPrimaryKey
  , testThis "Apple has the expected table name" testSpecifiedTableName
  , testThis "Apple has the expected fields" testInferredFields
  , testThis "Prefix stripping works" testPrefixStrippingWorks
  , testThis "Explicit schema works" testExplicitSchemaWorks
  , testThis "Generically derived schema works" testGenericallyDerivedSchema
  ]


testExpectedTableName :: TestM ()
testExpectedTableName = 
    U.assertEqual (tableName @TestType) "test_type"

testExpectedFieldList :: TestM ()
testExpectedFieldList =
    U.assertEqual (fields @TestType)  [[field| field_one |], [field| field_two |], [field| field_three |]]

testExpectedPrimaryKey :: TestM ()
testExpectedPrimaryKey =
    U.assertEqual (primaryKey @TestType) (Field "field_one" Nothing)

testInferredPrimaryKey :: TestM ()
testInferredPrimaryKey =
    U.assertEqual (primaryKey @Apple) (Field "this_field" Nothing)

testSpecifiedTableName :: TestM ()
testSpecifiedTableName =
    U.assertEqual (tableName @Apple) "apples"

testInferredFields :: TestM ()
testInferredFields =
    U.assertEqual (fields @Apple) [[field| this_field |], [field| that_field |]]

testPrefixStrippingWorks :: TestM ()
testPrefixStrippingWorks =
  U.assertEqual (fields @Endpoint) [[field| id |], [field| project_id |], [field| request_hashes |]]

testExplicitSchemaWorks :: TestM ()
testExplicitSchemaWorks =
  U.assertEqual (getTableName @Tags) "public.\"tags\""

testGenericallyDerivedSchema :: TestM ()
testGenericallyDerivedSchema =
  U.assertEqual (getTableName @Endpoint) "apis.\"endpoints\""
