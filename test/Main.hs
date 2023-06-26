module Main where

import Data.Pool (withResource)
import qualified EntitySpec
import qualified GenericsSpec
import Optics.Core
import Test.Tasty (TestTree, defaultMain, testGroup)
import Utils

main :: IO ()
main = do
  env <- getTestEnvironment
  withResource (env ^. #pool) migrate
  spec <- traverse (`runTestM` env) specs
  defaultMain . testGroup "pg-entity tests" $ spec

specs :: [TestM TestTree]
specs =
  [ GenericsSpec.spec
  , EntitySpec.spec
  ]

getTestEnvironment :: IO TestEnv
getTestEnvironment = do
  TestConfig{..} <- retrieveTestEnv
  pool <- mkPool connectionInfo 100 10
  pure TestEnv{..}
