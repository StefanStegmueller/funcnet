module Loss
  ( Loss (..),
    squaredError,
    binaryCrossEntropy,
  )
where

import Util

type Loss = Function (Double -> Double -> Double)

squaredError :: Loss
squaredError =
  Function
    { _func = squaredError',
      _deriv = squaredErrorDeriv
    }

squaredError' :: Double -> Double -> Double
squaredError' y t = (t - y) ^ 2

squaredErrorDeriv :: Double -> Double -> Double
squaredErrorDeriv y t = 2 * (y - t)

binaryCrossEntropy :: Loss
binaryCrossEntropy =
  Function
    { _func = binaryCrossEntropy',
      _deriv = binaryCrossEntropyDeriv
    }

binaryCrossEntropy' :: Double -> Double -> Double
binaryCrossEntropy' y t = - ((t * log y) + ((1 - t) * log (1 - y)))

binaryCrossEntropyDeriv :: Double -> Double -> Double
binaryCrossEntropyDeriv y t = (t - y) / (y * (1 - y))
