{-# LANGUAGE TemplateHaskell #-}

module Training
  ( TrainParams (..),
    train,
    gradientDescent,
    shuffleData,
  )
where

import Control.Lens hiding (pre)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Linalg hiding (transpose)
import Network
import Util (shuffle)

data TrainParams = TrainParams
  { _inps :: Matrix,
    _lbls :: Matrix,
    _epochs :: Int,
    _batchSize :: Int,
    _opt :: Matrix -> Matrix -> Matrix
  }

makeLenses ''TrainParams

gradientDescent :: Double -> Matrix -> Matrix -> Matrix
gradientDescent alpha grad mat = mat `sub` (alpha `smul` grad)

train :: TrainParams -> Network -> IO Network
train params net = train' params net $ params ^. epochs
train' params net 0 = return net
train' params net e = do
  (modX, modT) <- shuffleData (params^.inps, params^.lbls)
  let modParams = params & inps .~ modX
                         & lbls .~ modT
  let (nextNet, loss) = trainEpoch modParams net
  putStrLn $ "Epoch " ++ show ((params ^. epochs) - e + 1) ++ ":"
  putStrLn $ "Loss -> " ++ show loss
  train' params nextNet $ e - 1

shuffleData :: (Matrix, Matrix) -> IO (Matrix, Matrix)
shuffleData (x, t) = do 
  shuffledData <- shuffle $ zip x t
  return $ unzip shuffledData

trainEpoch :: TrainParams -> Network -> (Network, Double)
trainEpoch params net = (trainedNet, (1 / fromIntegral (length losses)) * sum losses)
  where
    batches = chunksOf (params ^. batchSize) $ zip (params ^. inps) (params ^. lbls)
    (trainedNet, losses) = foldl (trainBatch params) (net, []) batches

-- | Returns network with updated weights, biases and no input layer
trainBatch :: TrainParams -> (Network, [Double]) -> [([Double], [Double])] -> (Network, [Double])
trainBatch params (net, losses) batch = (net & layers .~ updatedDenseLyrs, losses ++ newLosses)
  where
    (gradients, newLosses) = processBatch net batch
    updateWeights d g =
      Dense
        ( d & weights %~ (params ^. opt) (g ^. dw)
            & bias %~ (params ^. opt) (g ^. db)
        )
    updatedDenseLyrs = zipWith updateWeights ([d | Dense d <- net ^. layers]) gradients

processBatch :: Network -> [([Double], [Double])] -> ([Gradient], [Double])
processBatch net batch = (map normalize sumGrads, losses)
  where
    normalize g =
      g & dw %~ smul (1 / fromIntegral (length batch))
        & db %~ smul (1 / fromIntegral (length batch))
    (gradients, losses) = unzip $ map (processStep net) batch
    modGrad sum gr =
      sum & dw %~ add (gr ^. dw)
        & db %~ add (gr ^. db)
    sumGrads = map (foldl1 modGrad) $ transpose gradients

processStep :: Network -> ([Double], [Double]) -> ([Gradient], Double)
processStep net (x, t) = (gradients, loss)
  where
    newInputNet inp n = n & layers .~ (Input inp : [Dense d | Dense d <- n ^. layers])
    forwardedNet = feedForward $ newInputNet [x] net
    gradients = backprop forwardedNet [t]
    loss = computeLoss forwardedNet [t]
