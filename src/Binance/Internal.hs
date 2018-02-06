{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Binance.Internal where

import Debug.Trace

import Utils
import Types (Opts(..))
import Binance.Types

import           Network.Wreq
import           Network.Connection  (TLSSettings (..))
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Control.Lens 
import           Control.Exception as E
import           Data.Aeson.Lens 
import           Data.Aeson
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Data.List (intercalate)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Crypto.Hash.SHA256 as SHA256


-- HELPER FUNCTIONS ---------------------------------------------------------------------------------------------------
getUrl :: Opts -> String
getUrl Opts{..} = intercalate "/" [ "https://api.binance.com"
                                  , "api"
                                  , optApiVersion
                                  , optPath ]

apiSign :: Opts -> ByteString
apiSign Opts{..} = B16.encode $ SHA256.hmac optApiPrivKey totalParams
        where totalParams = fromParams optPost <> fromParams optParams

-- HEADERS -------------------------------------------------------------------------------------------------------------
getDefaults :: Opts -> Network.Wreq.Options
getDefaults opts@Opts{..} = defaults & header "Accept" .~ ["application/json"] 
                                     & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
                                     & params .~ optParams 

postDefaults :: Opts -> Network.Wreq.Options
postDefaults opts@Opts{..} = getDefaults opts & header "X-MBX-APIKEY" .~ [optApiPubKey]
                                              & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
                                              & params .~ optParams <> [("signature", decodeUtf8 (apiSign opts))]

-- HTTP CALLS -----------------------------------------------------------------------------------------------------------
runGetApi :: FromJSON r => Opts -> IO (Either String r)
runGetApi = runGetApi' getDefaults

runGetPrivApi :: FromJSON r => Opts -> IO (Either String r)
runGetPrivApi = runGetApi' postDefaults 

runGetApi' :: FromJSON r => (Opts -> Network.Wreq.Options) -> Opts -> IO (Either String r)
runGetApi' fOpts opts@Opts{..} = do
        let opts' = fOpts opts
            url = getUrl opts
        (getWith opts' url >>= asValue >>= handleRes) `E.catch` handleExcept

runPostApi :: FromJSON r => Opts -> IO (Either String r)
runPostApi opts@Opts{..} = do
        let opts' = postDefaults opts        
            url = getUrl opts
            body = toFormParam optPost
        (postWith opts' url body >>= asValue >>= handleRes) `E.catch` handleExcept

-- HANDLERS ------------------------------------------------------------------------------------------------------------
handleExcept :: FromJSON r => HttpException -> IO (Either String r)
handleExcept e = return $ Left $ "Network Exception: " ++ show e

handleRes :: FromJSON b => Response Value -> IO (Either String b)
handleRes res = do
        let p = res ^. responseBody 
        case fromJSON p of
          Success s -> return $ Right s
          Error e -> return $ Left $ "Parse Error: " ++ e


        
