{-# LANGUAGE TemplateHaskell #-}

module Network 
  ( Layer (..)
  , activation
  , pre
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
                   , _pre :: Matrix
                   , _out :: Matrix
                   , _weights :: Matrix } 

data Network = Network { _layers :: [Layer]
                       , _loss :: Loss }

$(makeLenses ''Layer)
$(makeLenses ''Network)


forward :: Network -> Matrix -> Network
forward net inp = net & layers .~ (tail $ foldl feedLayer [dummyLyr] $ net ^. layers)
  where dummyLyr = Layer { _activation = sigmoid
                         , _pre = (zeros 1 1)
                         , _out = inp
                         , _weights = (zeros 1 1) }
        modPreAct lastOut lyr = lyr & pre.~ lastOut `matmul` (lyr ^. weights)
        modOut lyr = lyr & out .~ (apply (lyr ^. activation . func) $ (lyr ^. pre))
        feedLayer modlyrs lyr = modlyrs ++ [modOut $ modPreAct (last modlyrs ^. out) lyr] 

backprop :: Network -> Matrix -> Matrix -> [Matrix]
backprop net x t = map transpose $ [delta `matmul` x] ++ grads
  where (outLyr : lyrs) = reverse $ net ^. layers 
        dE_aL = elementWiseOp (net ^. loss . deriv) (outLyr ^. out) t
        deltaL = transpose $ hadamard (apply (outLyr ^. activation . deriv) $ outLyr ^. pre) dE_aL 
        compGradients (grads, delta, lastWeights) lyr =  
                ( [matmul delta $ lyr ^. out] ++ grads
                , hadamard (transpose $ apply (lyr ^. activation . deriv) (lyr ^. pre)) (matmul lastWeights delta)
                , lyr ^. weights)
        (grads, delta, lastWeights) = foldl compGradients ([], deltaL, outLyr ^. weights) lyrs
