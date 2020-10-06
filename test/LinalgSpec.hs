module LinalgSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Vector as V
import Linalg
import Test.Hspec
import Util (fromList2)

-- test transpose matrices
m1 = fromList2 [[1, 2], [3, 4]]

m2 = fromList2 [[1, 3], [2, 4]]

m3 = fromList2 [[1, 2], [1, 2, 3]]

-- test add, sub, hadamard matrices
m4 = fromList2 [[1, 2], [2, 1]]

m5 = fromList2 [[2, 1], [1, 2]]

m6 = fromList2 [[3, 3], [3, 3]]

m7 = fromList2 [[1, 2], [1]]

m8 = fromList2 [[1], [1]]

m9 = fromList2 [[-1, 1], [1, -1]]

m16 = fromList2 [[2, 2], [2, 2]]

-- test matmul matrices
m10 = fromList2 [[1, 2], [2, 1], [1, 2]]

m11 = fromList2 [[2, 1, 2], [1, 2, 1]]

m12 = fromList2 [[4, 5, 4], [5, 4, 5], [4, 5, 4]]

m13 = fromList2 [[1], [1, 2]]

-- test smul matrices
m14 = fromList2 [[1, 2], [3, 4]]

m15 = fromList2 [[2, 4], [6, 8]]

spec :: Spec
spec = do
  describe "transpose" $ do
    it "returns the correct transposed matrix" $
      transpose m1 `shouldBe` m2

    it "throws an exception because of inconsistent matrix" $
      evaluate (transpose m3) `shouldThrow` anyErrorCall

  describe "add" $ do
    it "returns the correct sum" $
      add m4 m5 `shouldBe` m6

    it "throws an exception because of inconsistent matrix" $
      evaluate (add m7 m8) `shouldThrow` anyErrorCall

    it "throws an exception because of different matrix dimensions" $
      evaluate (add m4 m8) `shouldThrow` anyErrorCall

  describe "sub" $ do
    it "returns the correct subtraction" $
      sub m4 m5 `shouldBe` m9

    it "throws an exception because of inconsistent matrix" $
      evaluate (sub m7 m8) `shouldThrow` anyErrorCall

    it "throws an exception because of different matrix dimensions" $
      evaluate (sub m4 m8) `shouldThrow` anyErrorCall

  describe "matmul" $ do
    it "returns the correct product" $
      matmul m10 m11 `shouldBe` m12

    it "throws an exception because of inconsistent matrix" $
      evaluate (matmul m7 m13) `shouldThrow` anyErrorCall

    it "throws an exception because matrix dimensions not matching" $ do
      evaluate (m10 `matmul` m10) `shouldThrow` anyErrorCall

  describe "smul" $ do
    it "returns the correct product" $
      smul 2 m14 `shouldBe` m15

    it "throws an exception because of inconsistent matrix" $
      evaluate (smul 2 m7) `shouldThrow` anyErrorCall

  describe "hadamard" $ do
    it "returns the correct hadamard product" $
      hadamard m4 m5 `shouldBe` m16

    it "throws an exception because of inconsistent matrix" $
      evaluate (hadamard m7 m8) `shouldThrow` anyErrorCall

    it "throws an exception because of different matrix dimensions" $
      evaluate (hadamard m4 m8) `shouldThrow` anyErrorCall
