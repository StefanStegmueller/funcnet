module Activation
  ( Activation,
    sigmoid,
    tanh,
    relu,
    softmax,
  )
where

import qualified Data.Vector as V
import Linalg
import Util
import Prelude hiding (tanh)

type Activation = Function (Matrix -> Matrix)

sigmoid :: Activation
sigmoid =
  Function
    { _func = apply sigmoid',
      _deriv = apply sigmoidDeriv
    }

sigmoid' :: Double -> Double
sigmoid' x = 1 / (+) 1 (exp $ - x)

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = sigmoid' x * (1 - sigmoid' x)

tanh :: Activation
tanh =
  Function
    { _func = apply tanh',
      _deriv = apply tanhDeriv
    }

tanh' :: Double -> Double
tanh' x = num / denom
  where
    num = exp x - exp (- x)
    denom = exp x + exp (- x)

tanhDeriv :: Double -> Double
tanhDeriv x = 1 - tanh' x ^ 2

relu :: Activation
relu =
  Function
    { _func = apply relu',
      _deriv = apply reluDeriv
    }

relu' :: Double -> Double
relu' x
  | x <= 0 = 0
  | otherwise = x

reluDeriv :: Double -> Double
reluDeriv x
  | x <= 0 = 0
  | otherwise = 1

softmax :: Activation
softmax =
  Function
    { _func = softmax',
      _deriv = softmaxDeriv
    }

softmax' :: Matrix -> Matrix
softmax' m = V.singleton $ V.map (\x -> exp x / denom) vec
  where
    denom = V.sum $ V.map exp vec
    vec = V.head m

softmaxDeriv :: Matrix -> Matrix
softmaxDeriv m = V.singleton $ V.zipWith term vec (vecIndex vec)
  where
    term x i = (exp x * V.sum (V.zipWith (\n j -> if j /= i then exp n else 0.0) vec (vecIndex vec))) / sumSquared
    sumSquared = V.sum (V.map exp vec) ** 2
    vecIndex v = (V.enumFromN 0 $ V.length v)
    vec = V.head m
