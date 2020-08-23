module Loss 
        ( Loss(..)
        , squaredError
        , squaredErrorDeriv
        ) where

import Util

type Loss = Function (Double -> Double -> Double)

squaredError :: Loss
squaredError = Function {func = squaredError', deriv = squaredErrorDeriv}

squaredError' :: Double -> Double -> Double
squaredError' y t = (t-y)^2

squaredErrorDeriv :: Double -> Double -> Double
squaredErrorDeriv y t = 2 * (y - t)

