module Init
  ( stdNormal
  , xavier 
  , he
  ) where

import Linalg
import Data.Random.Normal
import Data.List.Split (chunksOf)

he :: Int -> Int -> Int -> Matrix
he n m seed = genNormal n m seed (0, sqrt $ 2 / (fromIntegral n))

xavier :: Int -> Int -> Int -> Matrix
xavier  n m seed = genNormal n m seed (0, sqrt $ 1 / (fromIntegral n))

stdNormal :: Int -> Int -> Int -> Matrix
stdNormal n m seed = genNormal n m seed (0, 1)

genNormal :: Int -> Int -> Int -> (Double, Double) -> Matrix
genNormal n m seed params = chunksOf m randoms
  where randoms = take (n * m) $ mkNormals' params seed 

