module SimpleTrain where

import Prelude hiding (tanh)
import Control.Lens

import Network 
import Linalg
import Init
import Activation
import Loss
import Training

-- Data x and labels t
x = [[1, 1]
    ,[0, 1]
    ,[1, 0]
    ,[0, 0]]

t = [[0]
    ,[1]
    ,[1]
    ,[0]]

main :: IO ()
main = do
  let net = initNetwork
  let p = hParams 
  train p net
  return ()

initNetwork :: Network 
initNetwork = Network layers squaredError  
  where init = (he 1234)
        layers = compose [ (Input x      , 2, init)
                         , (dense relu   , 2, init)
                         , (dense tanh   , 1, init)
                         , (dense sigmoid, 3, init)
                         , (dense sigmoid, 1, init)]

hParams :: TrainParams
hParams = TrainParams { _inps = x
                      , _lbls = t
                      , _epochs = 3
                      , _batchSize = 2
                      , _opt = (gradientDescent 0.03)}
