{-# LANGUAGE TemplateHaskell #-}

module Network 
  ( Layer (..)
  , activation
  , preActivation
  , out
  , weights
  , Network (..)
  , layers
  , loss
  , forward
  , backprop
  ) where

import Activation
import Loss
import Linalg
import Util
import Lens.Simple

data Layer = Layer { _activation :: Activation
                   , _preActivation :: Matrix
                   , _out :: Matrix
                   , _weights :: Matrix } 

data Network = Network { _layers :: [Layer]
                       , _loss :: Loss }

$(makeLenses ''Layer)
$(makeLenses ''Network)

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst

forward :: Network -> Matrix -> (Network, Matrix)
forward net inp = (net & layers .~ modlyrs, y)
  where preactivation input lyr = input `matmul` (lyr ^. weights)
        modlyr lyr preact o = lyr & preActivation .~ preact
                                  & out .~ o
        feedLayer (modlyrs, x) lyr = 
                ( modlyrs ++ [modlyr lyr (preactivation x lyr) (apply (lyr ^. activation . func) $ preactivation x lyr)] 
                , apply (lyr ^. activation . func) $ preactivation x lyr)
        (modlyrs, y) = foldl feedLayer ([], inp) $ net ^. layers 

backprop :: Network -> Matrix -> Matrix -> Matrix -> [Matrix]
backprop net x y t = map transpose $ [delta `matmul` x] ++ grads
  where (outLyr : lyrs) = reverse $ net ^. layers 
        dE_aL = elementWiseOp (net ^. loss . deriv) y t
        deltaL = transpose $ hadamard (apply (outLyr ^. activation . deriv) $ outLyr ^. preActivation) dE_aL 
        compGradients (grads, delta, lastWeights) lyr =  
                ( [matmul delta $ lyr ^. out] ++ grads
                , hadamard (transpose $ apply (lyr ^. activation . deriv) (lyr ^. preActivation)) (matmul lastWeights delta)
                , lyr ^. weights)
        (grads, delta, lastWeights) = foldl compGradients ([], deltaL, outLyr ^. weights) lyrs
