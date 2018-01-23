{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson
import Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import GHC.Generics

data Currency = EUR
              | BTC
              | XRP
              | ETH
              | LTC
              | UNKNOWN Text
        deriving(Show,Eq,Read,Generic) 

instance FromJSON Currency

data MarketName = MarketName Currency Currency
        deriving (Eq,Read)

newtype Balance = Balance [(Currency,Float)]

data Ticker = Ticker {
                       bid :: Float
                     , ask :: Float
                     , last :: Float
                     }
               deriving (Eq,Show)
