module Util 
        ( Function (..)
        , fst3)
        where

data Function a = Function { func :: a
                           , deriv :: a }

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
