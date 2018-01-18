{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Kraken.Types where

import Data.Maybe
import Data.Text hiding (read,head)
import Data.Time
import Data.Time.ISO8601
import Data.Aeson
import Text.Read


type Params = [(Text,Text)]

data Opts = Opts {
                   optPath    :: String
                 , optParams  :: Params 
                 , optApiType :: String
                 }


data Ticker = Ticker {
                       bid :: Float
                     , ask :: Float
                     , last :: Float
                     }
               deriving (Eq,Show)

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                bid <- o .: "b"
                ask <- o .: "a"
                last <- o .: "c"
                pure $ Ticker (read $ head bid)  (read $ head ask) (read $ head last) 
