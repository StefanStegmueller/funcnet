module Activation
  ( Activation,
    sigmoid,
    tanh,
    relu,
    softmax,
  )
where

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
softmax' m = [map (\x -> exp x / denom) vec]
  where
    denom = sum $ map exp vec
    vec = head m

softmaxDeriv :: Matrix -> Matrix
softmaxDeriv m = [zipWith term vec [0 ..]]
  where
    term x i = (exp x * sum [exp n | (n, j) <- zip vec [0 ..], j /= i]) / sumSquared
    sumSquared = sum (map exp vec) ** 2
    vec = head m
