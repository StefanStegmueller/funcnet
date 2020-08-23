module Util 
        ( Function (..))
        where

data Function a = Function { func :: a
                           , deriv :: a }
