{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bittrex.Internal where

import           Bittrex.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.List       (intercalate)
import qualified Data.Text       as Text
import           Network.Wreq

runApi :: FromJSON r => Opts -> IO (Either String r)
runApi Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] & params .~  optParams
        let url = intercalate "/" [ "https://bittrex.com/api"
                                  , "v1.1"
                                  , optApiType
                                  , optPath ]
        res <- getWith opts' url
        let Just (Bool success) = res ^? (responseBody . key "success")
        let msg = res ^. responseBody . key "message" . _String
        let Just p = res ^? responseBody . key "result"
        if success then case fromJSON p of
                          Success s -> return $ Right s
                          Error e   -> return $ Left $ "Parse Error: " ++ e
                   else return $ Left $ "Network Error: " ++ Text.unpack msg
