{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Bittrex.Types where

import Debug.Trace

import Types

import Prelude as P
import Data.Maybe
import Data.Text as Text
import Data.Time
import Data.Time.ISO8601
import Data.Aeson
import Data.List.Split (splitOn)
import Text.Read

type Params = [(Text,Text)]

data Opts = Opts {
                   optPath    :: String
                 , optParams  :: Params 
                 , optApiType :: String
                 }

newtype Time = Time UTCTime 
        deriving (Eq,Show)

instance FromJSON Time where
        parseJSON = withText "Time" $ \ t -> pure $ Time $ fromJust $ parseISO8601 $ Text.unpack t ++ ['z']

instance Show MarketName where
        show (MarketName a b) = P.concat [show a,"-",show b] 

instance FromJSON MarketName where
        parseJSON = withText "MarketName" $ \t -> do
                let [t1,t2] = Text.splitOn "-" t
                case readMaybe . Text.unpack <$> [t1,t2] of
                      [Nothing, Nothing] -> pure $ MarketName (UNKNOWN t1) (UNKNOWN t2)
                      [Nothing, Just c]  -> pure $ MarketName (UNKNOWN t1) c
                      [Just c, Nothing]  -> pure $ MarketName c (UNKNOWN t2)
                      [Just c1, Just c2] -> pure $ MarketName c1 c2


instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                bid <- o .: "Bid"
                ask <- o .: "Ask"
                last <- o .: "Last"
                pure Ticker{..}

data Market = Market {
                       currency         :: Text
                     , baseCurrency     :: Text
                     , currencyLong     :: Text
                     , baseCurrencyLong :: Text
                     , minTradeSize     :: Float
                     , marketName       :: MarketName
                     , isActive         :: Bool
                     , created          :: Time
                     }
        deriving (Eq,Show)

instance FromJSON Market where
        parseJSON = withObject "Market" $ \o -> do
                currency <- o .: "MarketCurrency"
                baseCurrency <- o .: "BaseCurrency"
                currencyLong <- o .: "MarketCurrencyLong"
                baseCurrencyLong <- o .: "BaseCurrencyLong"
                minTradeSize <- o .: "MinTradeSize"
                marketName <- o .: "MarketName"
                isActive <- o .: "IsActive"
                created <- o .: "Created"
                pure Market{..}

