module Main where

import qualified EntitySpec
import qualified GenericsSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  GenericsSpec.spec
  EntitySpec.spec
