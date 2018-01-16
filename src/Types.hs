{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Data.Maybe
import Data.Text
import Data.Time
import Data.Time.ISO8601
import Data.Aeson
import Text.Read

newtype Time = Time UTCTime 
        deriving (Eq,Show)

instance FromJSON Time where
        parseJSON = withText "Time" $ \ t -> pure $ Time $ fromJust $ parseISO8601 $ unpack t ++ ['z']

data MarketName = MarketName MarketName' | NewMarket Text
        deriving (Eq,Show)

instance FromJSON MarketName where
        parseJSON = withText "MarketName" $ \ t ->
                case readMaybe $ unpack $ replace "-" "_" t of
                  Nothing -> pure $ NewMarket t
                  Just m  -> pure $ MarketName m

data MarketName' = BTC_ETH
        deriving (Eq,Show,Read)

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


