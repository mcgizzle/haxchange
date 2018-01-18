{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Kraken.Internal where

import Kraken.Types

import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson
import Data.Monoid
import qualified Data.Text as Text
import Data.List (intercalate)

runApi :: FromJSON r => Opts -> IO (Either String r)
runApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] & params .~  optParams
        let url = intercalate "/" [ "https://api.kraken.com/0"
                                  , optApiType
                                  , optPath ]
        res <- getWith opts' url
        let err = Text.unpack $ res ^. responseBody . key "error" . _String
        let Just p = res ^? responseBody . key "result" . members 
        if null err then case fromJSON p of
                          Success s -> return $ Right s
                          Error e -> return $ Left $ "Parse Error: " ++ e
                   else return $ Left $ "Network Error: " ++ err       
