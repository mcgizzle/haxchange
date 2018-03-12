{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Haxchange.Binance.Internal where


import           Types                   (Error (..), Opts (..))
import           Utils

import           Control.Exception       as E
import           Control.Lens
import           Crypto.Hash.SHA256      as SHA256
import           Data.Aeson
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Base16  as B16
import           Data.List               (intercalate)
import           Data.Monoid             ((<>))
import           Data.Text.Encoding      (decodeUtf8)
import           Network.Connection      (TLSSettings (..))
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.Wreq


-- HELPER FUNCTIONS ---------------------------------------------------------------------------------------------------
getUrl :: Opts -> String
getUrl Opts{..} = intercalate "/" [ "https://api.binance.com"
                                  , "api"
                                  , optApiVersion
                                  , optPath ]

apiSign :: Opts -> ByteString
apiSign Opts{..} = B16.encode $ SHA256.hmac optApiPrivKey
                              $ fromParams optPost <> fromParams optParams

-- HEADERS -------------------------------------------------------------------------------------------------------------
getDefaults :: Opts -> Network.Wreq.Options
getDefaults Opts{..} = defaults & header "Accept" .~ ["application/json"]
                                & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
                                & params .~ optParams

postDefaults :: Opts -> Network.Wreq.Options
postDefaults opts@Opts{..} = getDefaults opts & header "X-MBX-APIKEY" .~ [optApiPubKey]
                                              & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
                                              & params .~ optParams <> [("signature", decodeUtf8 (apiSign opts))]

-- HTTP CALLS -----------------------------------------------------------------------------------------------------------
runGetApi :: FromJSON j => Opts -> IO (Either Error j)
runGetApi = runGetApi' getDefaults

runGetPrivApi :: FromJSON j => Opts -> IO (Either Error j)
runGetPrivApi = runGetApi' postDefaults

runGetApi' :: FromJSON j => (Opts -> Network.Wreq.Options) -> Opts -> IO (Either Error j)
runGetApi' f opts = do
        let opts' = f opts
            url = getUrl opts
        (getWith opts' url >>= asValue >>= handleRes) `E.catch` handleExcept

runPostApi :: FromJSON j => Opts -> IO (Either Error j)
runPostApi opts@Opts{..} = do
        let opts' = postDefaults opts
            url = getUrl opts
            body = toFormParam optPost
        (postWith opts' url body >>= asValue >>= handleRes) `E.catch` handleExcept

-- HANDLERS ------------------------------------------------------------------------------------------------------------
handleRes :: FromJSON j => Response Value -> IO (Either Error j)
handleRes res = do
        let p = res ^. responseBody
        case fromJSON p of
          Success s -> return $ Right s
          Error e   -> return $ Left $ ParseError e



