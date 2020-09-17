module Init
  ( Initialization
  , stdNormal
  , xavier 
  , he
  ) where

import Linalg
import Data.Random.Normal
import Data.List.Split (chunksOf)

type Initialization = Int -> Int -> Int -> Matrix

he :: Initialization
he seed n m = genNormal seed n m (0, sqrt $ 2 / fromIntegral n)

xavier :: Initialization
xavier seed n m = genNormal seed n m (0, sqrt $ 1 / fromIntegral n)

stdNormal :: Initialization 
stdNormal seed n m = genNormal seed n m (0, 1)

genNormal :: Int -> Int -> Int -> (Double, Double) -> Matrix
genNormal seed n m params = chunksOf m randoms
  where randoms = take (n * m) $ mkNormals' params seed 

