module Main where

import Linalg

m1 :: Matrix
m1 = [[1, 2.5, 3],
      [4, 3.7, 4]]

m2 :: Matrix
m2 = [[1, 2.5],
      [4, 3.7],
      [6, 9.1]]

main :: IO ()
main = prettyPrint $ matmul m1 m2
