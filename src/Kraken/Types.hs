{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Kraken.Types where

import Types

import Data.Map
import Data.Maybe
import Data.Text hiding (read,head)
import Data.Time
import Data.Time.ISO8601
import Data.Aeson
import Text.Read
import Network.Wreq (FormParam)


type Params = [(Text,Text)]

data Opts = Opts {
                   optPath       :: String
                 , optParams     :: Params 
                 , optApiType    :: String
                 , optApiPubKey  :: String
                 , optApiPrivKey :: String
                 , optPost       :: [FormParam]
                 }


instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                bid <- o .: "b"
                ask <- o .: "a"
                last <- o .: "c"
                pure $ Ticker (t bid) (t ask) (t last) 
                        where t = read . head

