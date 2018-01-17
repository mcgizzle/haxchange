{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Internal where

import Types

import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson
import Data.Monoid
import Data.Text

runApi :: FromJSON r => Opts -> IO (Either String r)
runApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] & params .~  optParams
        res <- getWith opts' $ "https://bittrex.com/api/v1.1/public/"++ optPath
        let success = res ^. responseBody . key "success" . _String
        let msg = res ^. responseBody . key "message" . _String
        let Just p = res ^? responseBody . key "result"  
        let success' = unpack success
        if success' == "true" then case fromJSON p of
                                     Success s -> return $ Right s
                                     Error e -> return $ Left $ "Error: " ++ e
                              else return $ Left $ "Error: " ++ unpack msg
