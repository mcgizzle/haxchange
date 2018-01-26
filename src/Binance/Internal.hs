{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Binance.Internal where

import Binance.Types

import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson
import Data.Monoid
import Data.Maybe (isNothing)
import qualified Data.Text as Text
import Data.List (intercalate)

runApi :: FromJSON r => Opts -> IO (Either String r)
runApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & params .~  optParams
            url = intercalate "/" [ "https://api.binance.com"
                                  , "v1"
                                  , optPath ]
        res <- getWith opts' url
        let merrMsg = res ^? responseBody . key "msg" . _String
            mres = res ^? responseBody 
        if isNothing merrMsg then case fromJSON (fromJust mres) of
                                    Success s -> return $ Right s
                                    Error e -> return $ Left $ "Parse Error: " ++ e
                             else return $ Left $ "Network Error: " ++ Text.unpack msg       
