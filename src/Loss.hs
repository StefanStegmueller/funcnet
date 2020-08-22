module Loss 
        ( Loss
        , squaredError
        , squaredErrorDeriv
        ) where

data Loss = Loss { func :: Double -> Double -> Double
                 , deriv :: Double -> Double -> Double }

squaredError :: Loss
squaredError = Loss {func = squaredError', deriv = squaredErrorDeriv}

squaredError' :: Double -> Double -> Double
squaredError' y t = (t-y)^2

squaredErrorDeriv :: Double -> Double -> Double
squaredErrorDeriv y t = 2 * (y - t)

