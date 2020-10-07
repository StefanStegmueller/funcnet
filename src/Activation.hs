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

-- | Derivatives have to be either diagonal matrices or Jacobian's
type Activation = Function (Matrix -> Matrix)

sigmoid :: Activation
sigmoid =
  Function
    { _func = apply sigmoid',
      _deriv = toDiag . V.head . apply sigmoidDeriv
    }

sigmoid' :: Double -> Double
sigmoid' x = 1 / (+) 1 (exp $ - x)

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = sigmoid' x * (1 - sigmoid' x)

tanh :: Activation
tanh =
  Function
    { _func = apply tanh',
      _deriv = toDiag . V.head . apply tanhDeriv
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
      _deriv = toDiag . V.head . apply reluDeriv
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

-- TODO: fix correct implementation, currently is just passing values
softmaxDeriv :: Matrix -> Matrix
--softmaxDeriv m = generate2 l l distinction
--  where
--    vec = V.head m
--    l = V.length vec
--    distinction i j =
--      if i == j
--        then (vec V.! j) * (1 - (vec V.! j))
--        else - ((vec V.! j) * (vec V.! i))
softmaxDeriv m = toDiag $ V.replicate (V.length $ V.head m) 1
