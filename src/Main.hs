{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe
import Data.Text
import Data.Time
import Text.Read
import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson
import Data.Map as Map

data MarketName = MarketName MarketName' | NewMarket Text
        deriving (Eq,Show)

instance FromJSON MarketName where
        parseJSON = withText "MarketName" $ \ t -> do
                case readMaybe $ unpack $ replace "-" "_" t of
                  Nothing -> pure $ NewMarket t
                  Just m  -> pure $ MarketName m
        

data MarketName' = BTC_ETH
        deriving (Eq,Show,Read)

data Market 
        = Market {
                       currency     :: Text
                     , baseCurrency :: Text
                     , currencyLong :: Text
                     , baseCurrencyLong :: Text
                     , minTradeSize :: Float
                     , marketName :: MarketName
                     , isActive :: Bool
                     , created :: UTCTime
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
                pure (Market{..})

main :: IO ()
main = do
        let opts = defaults & header "Accept" .~ ["application/json"]
        res <- getWith opts "https://bittrex.com/api/v1.1/public/getmarkets" 
        let Just p = res ^? (responseBody . key "result")  
        case fromJSON p :: Result [Market] of
              Success s -> print "sucess"
              Error e -> print $ "Error: " ++ e
        return ()
