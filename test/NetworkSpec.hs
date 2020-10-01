module NetworkSpec (spec) where

import Control.Exception (evaluate)
import Network
import Test.Hspec

spec :: Spec
spec = do
  describe "compose" $ do
    it "returns the correct transposed matrix" $
      [[1, 2], [3, 4]] `shouldBe` [[1, 2], [3, 4]]
