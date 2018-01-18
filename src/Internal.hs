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
        let msg = res ^. responseBody . key "message" . _String
        let Just p = res ^? responseBody . key "result"  
        case fromJSON p of
          Success s -> return $ Right s
          Error e -> return $ Left $ "Parse Error: " ++ e
