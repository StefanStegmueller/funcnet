module SimpleNN where

import Prelude hiding (tanh)
import Control.Lens

import Network 
import Linalg
import Activation
import Loss

-- Artificial Neural Network Example

-- Initial weight matrices
_T = [[0.19, -0.92],
     [-0.42, -0.28]]

_U = [[0.61],
     [-1.5]]

_V = [[1.5, -0.81, -0.24]]

_W = [[-1.4, -0.81],
     [-2.2, -1.7],
     [-0.27, -0.73]]

-- Input x and label t
x = [[-1, 1]]
t = [[0, 1]]

main :: IO ()
main = do
  let net1 = initNetwork
  let net2 = feedForward net1 x 
  putStrLn "+++ Feeding forward +++"
  mapM_ prettyPrint $ [lyr ^. out | Dense lyr <- net2 ^. layers] 
  
  putStrLn "+++ Backprop ++++++++++"
  let gradients = backprop net2 t
  mapM_ prettyPrint gradients
  
initNetwork :: Network 
initNetwork = Network layers squaredError  
  where test = zeros 2 3
        layers = [ Input x
                 , Dense (DenseData relu test test _T)
                 , Dense (DenseData tanh test test _U)
                 , Dense (DenseData sigmoid test test _V)
                 , Dense (DenseData sigmoid test test _W)]
