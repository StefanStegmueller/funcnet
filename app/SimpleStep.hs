module SimpleStep where

import Activation
import Control.Lens
import Init
import Linalg
import Loss
import Network
import Util (fromList2)
import Prelude hiding (tanh)

-- Artificial Neural Network Example

-- Initial weight matrices
_T = fromList2
  [ [0.19, -0.92],
    [-0.42, -0.28]
  ]

_U = fromList2
  [ [0.61],
    [-1.5]
  ]

_V = fromList2 [[1.5, -0.81, -0.24]]

_W = fromList2
  [ [-1.4, -0.81],
    [-2.2, -1.7],
    [-0.27, -0.73]
  ]

-- Input x and label t
x = fromList2 [[-1, 1]]

t = fromList2 [[0, 1]]

main :: IO ()
main = do
  let net1 = initNetwork
  let net2 = feedForward net1
  putStrLn "+++ Feeding forward +++"
  mapM_ prettyPrint $ [lyr ^. out | Dense lyr <- net2 ^. layers]

  putStrLn "+++ Backprop ++++++++++"
  let gradients = backprop net2 t
  mapM_ prettyPrint [g ^. dw | g <- gradients]

initNetwork :: Network
initNetwork = Network layers squaredError
  where
    init = (he 1234)
    layers =
      customWeights $
        compose
          [ (Input x, 2, init),
            (dense relu, 2, init),
            (dense tanh, 1, init),
            (dense sigmoid, 3, init),
            (dense sigmoid, 2, init)
          ]

customWeights :: [Layer] -> [Layer]
customWeights (inp : lyrs) = inp : zipWith insert lyrs [_T, _U, _V, _W]
  where
    insert (Dense d) w = Dense (d & weights .~ w)
