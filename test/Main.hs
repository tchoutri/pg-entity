module Main where

import Test.Hspec
import qualified EntitySpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  EntitySpec.spec
