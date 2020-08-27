{-# LANGUAGE TemplateHaskell #-}

module Util 
        ( Function (..)
        , func
        , deriv
        , fst3
        , apply)
        where

import Control.Lens

data Function a = Function { _func :: a
                           , _deriv :: a }

$(makeLenses ''Function)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst
