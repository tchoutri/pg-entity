module Main where

import Data.Pool (createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import Database.Postgres.Temp (Config (..))
import qualified Database.Postgres.Temp as Postgres.Temp
import Database.Postgres.Temp.Internal
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
  eitherDb <- Postgres.Temp.startConfig customConfig
  case eitherDb of
    Right db -> do
      pool <-
        createPool
          (PG.connectPostgreSQL $ Postgres.Temp.toConnectionString db)
          PG.close
          1
          100000000
          50
      pure TestEnv{..}
    Left _ -> error "meh"

customConfig :: Config
customConfig =
  verboseConfig
    { postgresConfigFile =
        verbosePostgresConfig
          <> [ ("log_connections", "off")
             , ("log_disconnections", "off")
             ]
    }
