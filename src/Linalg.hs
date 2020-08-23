module Linalg 
    ( Matrix
    , zeros
    , rows
    , cols
    , prettyPrint
    , elementWiseOp
    , transpose
    , add
    , matmul
    , smul
    , hadamard
    ) where

type Matrix = [[Double]]

zeros :: Int -> Int -> Matrix
zeros n m = replicate n $ take m [0.0, 0.0 ..]

rows :: Matrix -> Int
rows m1 = length $ checkConsistency m1

cols :: Matrix -> Int
cols m1 = length $ head $ checkConsistency m1

getRow :: Matrix -> Int -> [Double]
getRow m1 r = m1 !! r

getColumn :: Matrix -> Int -> [Double]
getColumn m1 c = [row !! c | row <- m1]

prettyPrint :: Matrix -> IO()
prettyPrint mat = do
        mapM_ printRow mat
        putStrLn ""
  where toString lst = concatMap (\x -> show x ++ "\t") lst
        printRow row = putStrLn $ toString row

elementWiseOp :: (Double -> Double -> Double) -> Matrix -> Matrix -> Matrix
elementWiseOp = (zipWith . zipWith) 

checkConsistency :: Matrix -> Matrix
checkConsistency m1
  | inconsistent m1 = error "Matrix is not consistent."
  | otherwise = m1
  where cs = length $ head m1
        inconsistent mat = any (\x -> length x /= cs) mat 

checkSameDims :: (Matrix, Matrix) -> (Matrix, Matrix)
checkSameDims (m1, m2)
  | not $ sameDims m1 m2 = error "Matrices do not have the same dimensions." 
  | otherwise = (m1, m2) 
  where sameDims m1 m2 = rows m1 == rows m2 && cols m1 == cols m2

checkMatchingDims :: (Matrix, Matrix) -> (Matrix, Matrix)
checkMatchingDims (m1, m2)
  | not $ matchingDims m1 m2 = error "Matrices do not have matching dimensions for this operation."
  | otherwise = (m1, m2)
  where matchingDims m1 m2 = cols m1 == rows m2

transpose :: Matrix -> Matrix
transpose m1 = [getColumn sm1 j | j <- [0 .. m-1]]
  where sm1 = checkConsistency m1
        m = cols m1

add :: Matrix -> Matrix -> Matrix
add m1 m2 = add' $ checkSameDims (checkConsistency m1, checkConsistency m2) 
  where add' (mat1, mat2) = elementWiseOp (+) mat1 mat2 

matmul :: Matrix -> Matrix -> Matrix
matmul m1 m2 = matmul' $ checkMatchingDims (checkConsistency m1, checkConsistency m2)  
  where n = rows m1 
        m = cols m2 
        sumprod i j mat1 mat2 = sum $ zipWith (*) (getRow mat1 i) (getColumn mat2 j)
        matmul' (mat1, mat2) = [[sumprod i j mat1 mat2 | j <- [0 .. m-1]] | i <- [0 .. n-1]]

smul :: Double -> Matrix -> Matrix
smul s m1 = smul' $ checkConsistency m1 
  where smul' mat1 = [[s * e | e <- row] | row <- mat1] 

hadamard :: Matrix -> Matrix -> Matrix
hadamard m1 m2 = hadamard' $ checkSameDims (checkConsistency m1, checkConsistency m2)
  where hadamard' (mat1, mat2) = elementWiseOp (*) mat1 mat2
  
