{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Binance.Internal where

import Binance.Types

import           Network.Wreq
import           Network.Connection      (TLSSettings (..))
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Control.Lens 
import           Data.Aeson.Lens 
import           Data.Aeson
import           Data.Monoid
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Data.List (intercalate)

getUrl :: Opts -> String
getUrl Opts{..} = intercalate "/" [ "https://api.binance.com"
                                  , "api"
                                  , optApiVersion
                                  , optPath ]

runApi :: FromJSON r => Opts -> IO (Either String r)
runApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & params .~  optParams
                             & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            url = getUrl opts
        res <- asValue =<< getWith opts' url
        let p = res ^. responseBody 
        print p
        case fromJSON p of
          Success s -> return $ Right s
          Error e -> return $ Left $ "Parse Error: " ++ e
