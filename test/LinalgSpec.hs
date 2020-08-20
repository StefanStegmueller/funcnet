module LinalgSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Linalg 

spec :: Spec
spec = do
  describe "transpose" $ do
    it "returns the correct transposed matrix" $ 
      transpose [[1,2],[3,4]] `shouldBe` [[1,3],[2,4]]
      
    it "throws an exception because of inconsistent matrix" $ 
      evaluate (transpose [[1,2],[1,2,3]]) `shouldThrow` anyErrorCall 
      
  describe "add" $ do
    it "returns the correct sum" $ 
      add [[1,2], [2,1]] [[2,1], [1,2]] `shouldBe` [[3,3],[3,3]]

    it "throws an exception because of inconsistent matrix" $ 
      evaluate (add [[1,2], [1]] [[1], [1]]) `shouldThrow` anyErrorCall

    it "throws an exception because of different matrix dimensions" $ 
      evaluate (add [[1,2], [1,2]] [[1], [1]]) `shouldThrow` anyErrorCall

  describe "matmul" $ do
    it "returns the correct product" $ 
      matmul [[1,2], [2,1], [1,2]] [[2,1,2], [1,2,1]] `shouldBe` [[4,5,4],[5,4,5],[4,5,4]]

    it "throws an exception because of inconsistent matrix" $ 
      evaluate (matmul [[1,2],[1]] [[1],[1,2]]) `shouldThrow` anyErrorCall

    it "throws an exception because matrix dimensions not matching" $ do
      let mat = [[1,2,3],
                 [1,2,3]]
      evaluate (mat `matmul` mat) `shouldThrow` anyErrorCall 
      
  describe "smul" $ do
    it "returns the correct product" $ 
      smul 2 [[1,2],[3,4]] `shouldBe` [[2,4],[6,8]]
      
    it "throws an exception because of inconsistent matrix" $ 
      evaluate (smul 2 [[1,2], [1]]) `shouldThrow` anyErrorCall
   
  describe "hadamard" $ do
    it "returns the correct hadamard product" $ 
      hadamard [[1,2], [2,1]] [[2,1], [1,2]] `shouldBe` [[2,2],[2,2]]

    it "throws an exception because of inconsistent matrix" $ 
      evaluate (hadamard [[1,2], [1]] [[1], [1]]) `shouldThrow` anyErrorCall

    it "throws an exception because of different matrix dimensions" $ 
      evaluate (hadamard [[1,2], [1,2]] [[1], [1]]) `shouldThrow` anyErrorCall


