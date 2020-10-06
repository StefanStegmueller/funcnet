module Linalg
  ( Matrix,
    zeros,
    rows,
    cols,
    prettyPrint,
    elementWiseOp,
    transpose,
    add,
    sub,
    matmul,
    smul,
    hadamard,
  )
where

import qualified Data.Vector as V
import Prelude

type Matrix = V.Vector (V.Vector (Double))

zeros :: Int -> Int -> Matrix
zeros n m = V.replicate n $ V.fromList $ take m [0.0, 0.0 ..]

rows :: Matrix -> Int
rows m1 = V.length $ checkConsistency m1

cols :: Matrix -> Int
cols m1 = V.length $ V.head $ checkConsistency m1

getRow :: Matrix -> Int -> V.Vector (Double)
getRow m1 r = m1 V.! r

getColumn :: Matrix -> Int -> V.Vector (Double)
getColumn m1 c = V.map (flip (V.!) $ c) m1

prettyPrint :: Matrix -> IO ()
prettyPrint mat = do
  V.mapM_ printRow mat
  putStrLn ""
  where
    toString vec = concatMap (\x -> show x ++ "\t") vec
    printRow row = putStrLn $ toString row

elementWiseOp :: (Double -> Double -> Double) -> Matrix -> Matrix -> Matrix
elementWiseOp = (V.zipWith . V.zipWith)

checkConsistency :: Matrix -> Matrix
checkConsistency m1
  | inconsistent m1 = error "Matrix is not consistent."
  | otherwise = m1
  where
    cs = V.length $ V.head m1
    inconsistent mat = V.any (\x -> V.length x /= cs) mat

checkSameDims :: (Matrix, Matrix) -> (Matrix, Matrix)
checkSameDims (m1, m2)
  | not $ sameDims m1 m2 = error "Matrices do not have the same dimensions."
  | otherwise = (m1, m2)
  where
    sameDims m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

checkMatchingDims :: (Matrix, Matrix) -> (Matrix, Matrix)
checkMatchingDims (m1, m2)
  | not $ matchingDims m1 m2 = error "Matrices do not have matching dimensions for this operation."
  | otherwise = (m1, m2)
  where
    matchingDims m1 m2 = cols m1 == rows m2

transpose :: Matrix -> Matrix
transpose m1 = V.fromList [getColumn sm1 j | j <- [0 .. m -1]]
  where
    sm1 = checkConsistency m1
    m = cols m1

add :: Matrix -> Matrix -> Matrix
add m1 m2 = add' $ checkSameDims (checkConsistency m1, checkConsistency m2)
  where
    add' (mat1, mat2) = elementWiseOp (+) mat1 mat2

sub :: Matrix -> Matrix -> Matrix
sub m1 m2 = sub' $ checkSameDims (checkConsistency m1, checkConsistency m2)
  where
    sub' (mat1, mat2) = elementWiseOp (-) mat1 mat2

matmul :: Matrix -> Matrix -> Matrix
matmul m1 m2 = matmul' $ checkMatchingDims (checkConsistency m1, checkConsistency m2)
  where
    n = rows m1
    m = cols m2
    sumprod i j mat1 mat2 = sum $ V.zipWith (*) (getRow mat1 i) (getColumn mat2 j)
    matmul' (mat1, mat2) = V.fromList [V.fromList [sumprod i j mat1 mat2 | j <- [0 .. m -1]] | i <- [0 .. n -1]]

smul :: Double -> Matrix -> Matrix
smul s m1 = smul' $ checkConsistency m1
  where
    smul' mat1 = (V.map . V.map) ((*) s) mat1

hadamard :: Matrix -> Matrix -> Matrix
hadamard m1 m2 = hadamard' $ checkSameDims (checkConsistency m1, checkConsistency m2)
  where
    hadamard' (mat1, mat2) = elementWiseOp (*) mat1 mat2
