{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Binance.Internal where

import Debug.Trace

import Utils
import Types (Opts(..),Params)
import Binance.Types

import           Prelude as P
import           Util
import           Network.Wreq
import           Network.Connection      (TLSSettings (..))
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Control.Lens 
import           Data.Aeson.Lens 
import           Data.Aeson
import           Data.Monoid
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Data.List (intercalate)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Hex (hex)
import qualified Data.ByteString.Base16 as B16
import           Crypto.Hash.SHA256 as SHA256

getUrl :: Opts -> String
getUrl Opts{..} = intercalate "/" [ "https://api.binance.com"
                                  , "api"
                                  , optApiVersion
                                  , optPath ]

apiSign :: Opts -> ByteString
apiSign Opts{..} = B16.encode $ SHA256.hmac optApiPrivKey totalParams
        where totalParams = fromParams optPost <> fromParams optParams

fromParams :: Params -> ByteString
fromParams params = encodeUtf8 $ Text.intercalate "&" ((\(x,y) -> x <> "=" <> y) <$> params)

runGetApi :: FromJSON r => Opts -> IO (Either String r)
runGetApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & params .~  optParams
                             & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            url = getUrl opts
        getWith opts' url >>= asValue >>= handleRes

runGetPrivApi :: FromJSON r => Opts -> IO (Either String r)
runGetPrivApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & header "X-MBX-APIKEY" .~ [optApiPubKey]
                             & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
                             & params .~  optParams <> [("signature", Text.pack $ B8.unpack (apiSign opts))]
                             & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            url = getUrl opts 
        getWith opts' url >>= asValue >>= handleRes

runPostApi :: FromJSON r => Opts -> IO (Either String r)
runPostApi opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"]
                             & header "X-MBX-APIKEY" .~ [optApiPubKey]
                             & params .~ optParams
                             & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            url = getUrl opts
            body = ["signature" := apiSign opts] <> toFormParam optPost
        print opts'
        postWith opts' url body >>= asValue >>= handleRes

handleRes res = do
        let p = res ^. responseBody 
        print p
        case fromJSON p of
          Success s -> return $ Right s
          Error e -> return $ Left $ "Parse Error: " ++ e


        
