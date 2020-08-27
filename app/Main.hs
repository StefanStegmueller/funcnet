{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (index)

import Linalg

main :: IO ()
main = putStrLn "hello"

-- Lenses example

data Person = Person { _address:: Address 
                     , _index :: Int}

data Address = Address { _town:: String
                       , _street :: String
                       , _houseNr :: Int}

$(makeLenses ''Person)
$(makeLenses ''Address)

lensGet1 :: Person -> Int
lensGet1 p = view index p

lensGet2 :: Person -> Int 
lensGet2 p = p ^. index

lensGet3 :: Person -> String
lensGet3 p = p ^. address . street 

lensSet1 :: Person -> Person
lensSet1 p = set index 3 p 

lensSet2 :: Person -> Person
lensSet2 p = p & index .~ 3

-- view + apply function + set
lensOver1 :: Person -> Person
lensOver1 p = over index (\x -> x + 1) p

lensOver2 :: Person -> Person
lensOver2 p = p & index %~ (\x -> x + 1) 

-- Prisms Example

data User = Real RealData
          | Entity EntityData 

data RealData = RD { _name :: String , _age :: Int }
data EntityData = ED {_legalName :: String}

$(makeLenses ''RealData)
$(makeLenses ''EntityData)

test :: User 
test = Real RD { _name = "test", _age = 12  }




