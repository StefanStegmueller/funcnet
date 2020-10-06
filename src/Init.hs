module Init
  ( Initialization,
    stdNormal,
    xavier,
    he,
  )
where

import Data.Random.Normal
import qualified Data.Vector as V
import Data.Vector.Split (chunksOf)
import Linalg

type Initialization = Int -> Int -> Int -> Matrix

he :: Initialization
he seed n m = genNormal seed n m (0, sqrt $ 2 / fromIntegral n)

xavier :: Initialization
xavier seed n m = genNormal seed n m (0, sqrt $ 1 / fromIntegral n)

stdNormal :: Initialization
stdNormal seed n m = genNormal seed n m (0, 1)

genNormal :: Int -> Int -> Int -> (Double, Double) -> Matrix
genNormal seed n m params = V.fromList $ chunksOf m randoms
  where
    randoms = V.fromList $ take (n * m) $ mkNormals' params seed
