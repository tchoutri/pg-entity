module Main where

import qualified EntitySpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  EntitySpec.spec
