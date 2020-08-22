module Network 
  ( Layer
  , activation
  , preActivation
  , weights
  , Network
  , layers
  , loss
  ) where

import Activation
import Loss
import Linalg

data Layer = Layer { activation :: Activation
                   , preActivation :: Matrix
                   , weights :: Matrix} 

data Network = Network { layers :: [Layer]
                       , loss :: Loss}


apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst

forward :: Network -> Matrix -> Matrix
forward net inp = foldl feedLayer inp $ layers net
  where feedLayer x lyr = apply (func $ activation lyr) $ matmul x $ weights lyr

