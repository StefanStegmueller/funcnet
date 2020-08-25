{-# LANGUAGE TemplateHaskell #-}

module Util 
        ( Function (..)
        , func
        , deriv
        , fst3)
        where

import Lens.Simple

data Function a = Function { _func :: a
                           , _deriv :: a }

$(makeLenses ''Function)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
