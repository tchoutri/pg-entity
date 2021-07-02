{-# LANGUAGE OverloadedLists #-}
module GenericsSpec where

import Data.Text
import Database.PostgreSQL.Entity.Types
import GHC.Generics
import Test.Hspec

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

spec :: Spec
spec = describe "Ensure generically-derived instances with no options are correct" $ do
  it "TestType has the expected table name" $ do
    tableName @TestType `shouldBe` "test_type"
  it "TestType has the expected field list" $ do
    fields @TestType `shouldBe` ["field_one", "field_two", "field_three"]
  it "TestType has the expected primary key" $ do
    primaryKey @TestType `shouldBe` Field "field_one" Nothing
  it "Apple has the expected primary key" $ do
    primaryKey @Apple `shouldBe` Field "this_field" Nothing
  it "Apple has the expected table name" $ do
    tableName @Apple `shouldBe` "apples"
  it "Apple has the expected fields" $ do
    fields @Apple `shouldBe` ["this_field", "that_field"]
