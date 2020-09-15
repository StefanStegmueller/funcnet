{-# LANGUAGE TemplateHaskell #-}

module Network 
  ( Layer (..)
  , DenseData
  , activation
  , pre
  , out
  , weights
  , Network (..)
  , layers
  , loss
  , Gradient (..)
  , dw
  , db
  , dense
  , compose
  , feedForward
  , backprop
  ) where

import Control.Lens hiding (pre)

import Activation
import Loss
import Linalg
import Init
import Util

data DenseData = DenseData { _activation :: Activation
                   , _pre :: Matrix
                   , _out :: Matrix
                   , _weights :: Matrix
                   , _bias :: Matrix } 
makeLenses ''DenseData


data Layer = Input Matrix
           | Dense DenseData


data Network = Network { _layers :: [Layer]
                       , _loss :: Loss }
makeLenses ''Network


data Gradient = Gradient { _dw :: Matrix
                         , _db :: Matrix } 
makeLenses ''Gradient


dense :: Activation ->  Layer
dense act = Dense DenseData { _activation = act
                            , _pre = zeros 1 1
                            , _out = zeros 1 1
                            , _weights = zeros 1 1
                            , _bias = zeros 1 1}


compose :: [(Layer, Int, (Int -> Int -> Matrix))] -> [Layer]
compose lyrs = reverse $ compose' $ reverse lyrs 
compose' [] = []
compose' (x:xs) = (initLayer x (snd3 $ head xs) : compose' xs)
  where initLayer :: (Layer, Int, (Int -> Int -> Matrix)) -> Int -> Layer 
        initLayer (Dense d, m, init) n = Dense(d & weights .~ init n m
                                                 & bias    .~ zeros 1 m)
        initLayer (lyr, _, _) _        = lyr


feedForward :: Network -> Network
feedForward net = net & layers .~ map fst (foldl feedLayer [] $ net ^. layers)
  where feedLayer modlyrs lyr = modlyrs ++ [forward lyr $ snd $ last modlyrs] 


forward :: Layer -> Matrix -> (Layer, Matrix)
forward (Input x) _       = (Input x, x)
forward (Dense d) lastOut = (Dense modDense, modDense ^. out) 
  where modPreAct lout lyr = lyr & pre.~ lout `matmul` (lyr ^. weights) `add` (lyr ^. bias)
        modOut lyr = lyr & out .~ (lyr ^. activation . func) (lyr ^. pre)
        modDense = modOut $ modPreAct lastOut d


backprop :: Network -> Matrix -> [Gradient]
backprop net t = map transposeGrad $ [Gradient { _dw = delta `matmul` x, _db = delta } | Input x <- lyrs] ++ grads
  where (Dense outLyr : lyrs) = reverse $ net ^. layers 
        dE_aL = elementWiseOp (net ^. loss . deriv) (outLyr ^. out) t
        deltaL = transpose $ hadamard ((outLyr ^. activation . deriv) $ outLyr ^. pre) dE_aL 
        compGradients (grads, delta, lastWeights) lyr =  
                ( Gradient { _dw = matmul delta (lyr ^. out), _db = delta } : grads
                , hadamard (transpose $ (lyr ^. activation . deriv) (lyr ^. pre)) (matmul lastWeights delta)
                , lyr ^. weights)
        (grads, delta, lastWeights) = foldl compGradients ([], deltaL, outLyr ^. weights) [l | Dense l <- lyrs]
        transposeGrad grad = grad & dw %~ transpose
                                  & db %~ transpose
