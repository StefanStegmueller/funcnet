{-# LANGUAGE TemplateHaskell #-}

module Util 
        ( Function (..)
        , func
        , deriv
        , fst3
        , snd3
        , trd3
        , apply)
        where

import Control.Lens

data Function a = Function { _func :: a
                           , _deriv :: a }
$(makeLenses ''Function)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

apply :: (a -> b) -> [[a]] -> [[b]]
apply f lst = (map . map) f lst
