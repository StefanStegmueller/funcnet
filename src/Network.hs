module Network 
  ( Layer (..)
  , Network (..)
  , forward
  , backprop
  ) where

import Activation
import Loss
import Linalg
import Util

data Layer = Layer { activation :: Activation
                   , preActivation :: Matrix
                   , out :: Matrix
                   , weights :: Matrix } 

data Network = Network { layers :: [Layer]
                       , loss :: Loss }

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst

forward :: Network -> Matrix -> (Network, Matrix)
forward net inp = (Network modlyrs (loss net), y)
  where preactivation input lyr = input `matmul` (weights lyr)
        modlyr lyr preact out = Layer (activation lyr) preact out (weights lyr)
        feedLayer (modlyrs, x) lyr = 
                ( modlyrs ++ [modlyr lyr (preactivation x lyr) (apply (func $ activation lyr) $ preactivation x lyr)] 
                , apply (func $ activation lyr) $ preactivation x lyr)
        (modlyrs, y) = foldl feedLayer ([], inp) $ layers net 

backprop :: Network -> Matrix -> Matrix -> Matrix -> [Matrix]
backprop net x y t = map transpose $ [delta `matmul` x] ++ grads
  where (outLyr : lyrs) = reverse $ layers net 
        dE_aL = elementWiseOp (deriv $ loss net) y t
        deltaL = transpose $ hadamard (apply (deriv $ activation outLyr) $ preActivation outLyr) dE_aL 
        compGradients (grads, delta, lastWeights) lyr =  
                ( [matmul delta $ out lyr] ++ grads
                , hadamard (transpose $ apply (deriv $ activation lyr) (preActivation lyr)) (matmul lastWeights delta)
                , weights lyr)
        (grads, delta, lastWeights) = foldl compGradients ([], deltaL, weights outLyr) lyrs
