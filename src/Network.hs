module Network 
  ( Layer (..)
  , Network (..)
  , forward
  , backprop
  ) where

import Activation
import Loss
import Linalg

data Layer = Layer { activation :: Activation
                   , preActivation :: Matrix
                   , weights :: Matrix } 

data Network = Network { layers :: [Layer]
                       , loss :: Loss }

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst

forward :: Network -> Matrix -> (Network, Matrix)
forward net inp = (Network modlyrs (loss net), y)
  where preactivation input lyr = input `matmul` (weights lyr)
        modlyr lyr preact = Layer (activation lyr) preact (weights lyr)
        feedLayer (modlyrs, x) lyr = 
                ( modlyrs ++ [modlyr lyr $ preactivation x lyr] 
                , apply (func $ activation lyr) $ preactivation x lyr)
        (modlyrs, y) = foldl feedLayer ([], inp) $ layers net 

backprop :: Network -> Matrix -> Matrix -> [Matrix]
backprop net y t = [dE_aL]
  where dE_aL = elementWiseOp (deriv $ loss net) y t

