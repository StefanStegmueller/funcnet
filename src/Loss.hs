module Loss
  ( Loss (..),
    squaredError,
    crossEntropy,
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

crossEntropy :: Loss
crossEntropy =
  Function
    { _func = crossEntropy',
      _deriv = crossEntropyDeriv
    }

crossEntropy' :: Double -> Double -> Double
crossEntropy' y t = - t * (log y)

crossEntropyDeriv :: Double -> Double -> Double
crossEntropyDeriv y t = - (t / y)

binaryCrossEntropy :: Loss
binaryCrossEntropy =
  Function
    { _func = binaryCrossEntropy',
      _deriv = binaryCrossEntropyDeriv
    }

binaryCrossEntropy' :: Double -> Double -> Double
binaryCrossEntropy' y t = - ((t * (log y)) + ((1 - t) * (log (1 - y))))

binaryCrossEntropyDeriv :: Double -> Double -> Double
binaryCrossEntropyDeriv y t = (y - t) / (y * (1 - y))
