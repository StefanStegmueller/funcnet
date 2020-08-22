module Activation 
    ( Activation
    , func
    , deriv
    , sigmoid
    , sigmoidDeriv
    , tanh
    , tanhDeriv
    , relu
    , reluDeriv
    ) where

import Prelude hiding (tanh)

data Activation = Activation { func :: Double -> Double 
                             , deriv :: Double -> Double}

sigmoid :: Activation
sigmoid = Activation {func = sigmoid', deriv = sigmoidDeriv}

sigmoid' :: Double -> Double
sigmoid' x = 1 / (+) 1 (exp $ -x)

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = sigmoid' x * (1 - sigmoid' x)

tanh :: Activation
tanh = Activation {func = tanh', deriv = tanhDeriv}

tanh' :: Double -> Double
tanh' x = num / denum
  where num = exp x - exp (- x)
        denum = exp x + exp (- x)

tanhDeriv :: Double -> Double
tanhDeriv x = 1 - tanh' x ^ 2

relu :: Activation
relu = Activation {func = relu', deriv = reluDeriv}

relu' :: Double -> Double
relu' x
  | x <= 0 = 0
  | otherwise = x

reluDeriv :: Double -> Double
reluDeriv x
  | x <= 0 = 0
  | otherwise = 1
