{-# LANGUAGE TemplateHaskell #-}

module Util
  ( Function (..),
    func,
    deriv,
    fst3,
    snd3,
    trd3,
    apply,
    shuffle,
  )
where

import Control.Lens
import Control.Monad
import Data.Array.IO
import System.Random

data Function a = Function
  { _func :: a,
    _deriv :: a
  }

$(makeLenses ''Function)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst

-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs
