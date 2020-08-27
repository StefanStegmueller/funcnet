{-# LANGUAGE TemplateHaskell #-}

module Network 
  ( Layer (..)
  , DenseData(..)
  , activation
  , pre
  , out
  , weights
  , Network (..)
  , layers
  , loss
  , compose
  , feedForward
  , backprop
  ) where

import Control.Lens hiding (pre)

import Activation
import Loss
import Linalg
import Util

data Layer = Input Matrix
           | Dense DenseData

data DenseData = DenseData { _activation :: Activation
                   , _pre :: Matrix
                   , _out :: Matrix
                   , _weights :: Matrix } 
makeLenses ''DenseData

data Network = Network { _layers :: [Layer]
                       , _loss :: Loss }
makeLenses ''Network


compose :: [(Layer, Int)] -> [Layer]
compose lyrs = reverse $ compose' $ reverse lyrs 
compose' []     = []
compose' (x:xs) = ((initLayer x $ snd $ head xs) : compose' xs) 
  where initLayer :: (Layer, Int) -> Int -> Layer 
        initLayer ((Dense d), m) n = Dense(d & weights .~ zeros n m)
        initLayer (lyr, _) _       = lyr


feedForward :: Network -> Matrix -> Network
feedForward net inp = net & layers .~ (map fst $ foldl feedLayer [] $ net ^. layers)
  where feedLayer modlyrs lyr = modlyrs ++ [forward lyr $ snd $ last modlyrs] 


forward :: Layer -> Matrix -> (Layer, Matrix)
forward (Input x) _       = (Input x, x)
forward (Dense d) lastOut = (Dense(modDense), modDense ^. out) 
  where modPreAct lout lyr = lyr & pre.~ lastOut `matmul` (lyr ^. weights)
        modOut lyr = lyr & out .~ (apply (lyr ^. activation . func) $ (lyr ^. pre))
        modDense = modOut $ modPreAct lastOut d


backprop :: Network -> Matrix -> [Matrix]
backprop net t = map transpose $ [delta `matmul` x | Input x <- lyrs] ++ grads
  where (Dense outLyr : lyrs) = reverse $ net ^. layers 
        dE_aL = elementWiseOp (net ^. loss . deriv) (outLyr ^. out) t
        deltaL = transpose $ hadamard (apply (outLyr ^. activation . deriv) $ outLyr ^. pre) dE_aL 
        compGradients (grads, delta, lastWeights) lyr =  
                ( [matmul delta $ lyr ^. out] ++ grads
                , hadamard (transpose $ apply (lyr ^. activation . deriv) (lyr ^. pre)) (matmul lastWeights delta)
                , lyr ^. weights)
        (grads, delta, lastWeights) = foldl compGradients ([], deltaL, outLyr ^. weights) [l | Dense l <- lyrs]
