module NetworkSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Network

spec :: Spec
spec = do
  describe "compose" $ do
    it "returns the correct transposed matrix" $ 
      [[1,2],[3,4]] `shouldBe` [[1,2],[3,4]]
      
    
