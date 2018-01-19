{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Aeson
import Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)
import GHC.Generics

newtype Balance = Balance (Map.Map MarketName Float)
        deriving(Show,Generic)

data MarketName = MarketName MarketName' | NewMarket Text
        deriving (Eq,Show)

instance FromJSON MarketName where
        parseJSON = withText "MarketName" $ \ t ->
                case readMaybe $ Text.unpack $ Text.replace "-" "_" t of
                  Nothing -> pure $ NewMarket t
                  Just m  -> pure $ MarketName m

data MarketName' = BTC_ETH
        deriving (Eq,Show,Read)

data Ticker = Ticker {
                       bid :: Float
                     , ask :: Float
                     , last :: Float
                     }
               deriving (Eq,Show)


