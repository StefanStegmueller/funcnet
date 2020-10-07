module Main where

import Activation
import Control.Lens
import Data.Maybe
import Data.List.Split
import qualified Data.Vector as V
import Init
import Linalg
import Loss
import Network
import System.IO
import Training
import Util (fromList2)
import Prelude hiding (tanh)

dataPath = "./data/"

trainFileX = dataPath ++ "mnist_small_train_in.txt"

trainFileT = dataPath ++ "mnist_small_train_out.txt"

testFileX = dataPath ++ "mnist_small_test_in.txt"

testFileT = dataPath ++ "mnist_small_test_out.txt"

main :: IO ()
main = do
  -- Load data
  xTrain <- readInputData trainFileX
  tTrain <- readLabelData trainFileT
  xTest <- readInputData testFileX
  tTest <- readLabelData testFileT

  -- Initialize network and parameters
  let net = initNetwork
  let p = hParams xTrain tTrain

  -- Train network
  trainedNet <- train p net
  putStrLn "--- Training Complete ---"

  -- Predict
  predict xTest tTest trainedNet

readInputData :: String -> IO Matrix
readInputData fpath = do
  contents <- readFile fpath
  let strings = map (splitOn ",") $ lines contents
  let mat = (map . map) (\s -> read s :: Double) strings
  return $ fromList2 mat

readLabelData :: String -> IO Matrix
readLabelData fpath = do
  contents <- readFile fpath
  let labels = map (\s -> read s :: Integer) $ lines contents
  let onehot n = [if i == n then 1 else 0 | i <- [0 .. 9]]
  return $ fromList2 $ map onehot labels

initNetwork :: Network
initNetwork = Network layers crossEntropy
  where
    init = (he 1234)
    layers =
      compose
        [ (Input (zeros 1 1), 784, init),
          (dense relu, 100, init),
          (dense softmax, 10, init)
        ]

hParams :: Matrix -> Matrix -> TrainParams
hParams x t =
  TrainParams
    { _inps = x,
      _lbls = t,
      _epochs = 1,
      _batchSize = 300,
      _opt = (gradientDescent 0.03)
    }

predict :: Matrix -> Matrix -> Network -> IO ()
predict xTest tTest net = do
  putStrLn "Index of test sample to predict:"
  indexStr <- getLine
  let index = (read indexStr :: Int)
  let x = V.singleton $ xTest V.! index
  let t = fromJust $ V.findIndex (== 1.0) $ tTest V.! index
  let prediction = V.head $ (last [d | Dense d <- (feedForward $ insertInput x net) ^. layers]) ^. out
  let maxFilter (mi, max) (i, p) = if p > max then (i, p) else (mi, max)
  let (y, _) = V.foldl maxFilter (0, 0.0) $ V.zip (V.enumFromN 0 $ length prediction) prediction

  putStrLn $ "Prediction: " ++ show y ++ ", Truth: " ++ show t
  predict xTest tTest net
