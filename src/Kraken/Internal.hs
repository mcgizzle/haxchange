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
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as Byte
import Data.ByteString.Base64 as B64
import Crypto.Hash.SHA512 as SHA512
import Crypto.Hash.SHA256 as SHA256

get :: FromJSON r => Opts -> IO (Either String r)
get opts@Opts{..} = do
        let opts' = defaults & header "Accept" .~ ["application/json"] & params .~  optParams
        let url = intercalate "/" [ "https://api.kraken.com/0"
                                  , optApiType
                                  , optPath ]
        res <- getWith opts' url
        let err = res ^. responseBody . key "error" . _String
        let p = res ^? responseBody . key "result" . members 
        case p of
          Just p' -> case fromJSON p' of
                          Success s -> return $ Right s
                          Error e -> return $ Left $ "Parse Error: " ++ e
          Nothing -> return $ Left $ "Network Error: " ++  Text.unpack err      

post :: FromJSON r => Opts -> IO (Either String r)
post opts@Opts{..} = do
        nonce <- Prelude.head . splitOn "." . show <$> getPOSIXTime
        let body = [ "nonce" := nonce ] <> optPost
        let url = intercalate "/" [ "https://api.kraken.com/0"
                                  , optApiType
                                  , optPath ]
        let apisign = SHA512.hash $ urlBS <> nonceAndPost <> b64Api
            urlBS = Byte.pack url
            nonceAndPost = SHA256.hash $ Byte.pack (concat optPostData) <> Byte.pack nonce
            b64Api = B64.encode $ Byte.pack optApiPrivKey
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & header "API-Key" .~ [Byte.pack optApiPubKey] 
                             & header "API-Sign" .~ [apisign]
                             & params .~  optParams
        res <- postWith opts' url body
        print $ res ^. responseBody
        let err = res ^. responseBody . key "error" . _String
        let p = res ^? responseBody . key "result" . members 
        case p of
          Just p' -> case fromJSON p' of
                          Success s -> return $ Right s
                          Error e -> return $ Left $ "Parse Error: " ++ e
          Nothing -> return $ Left $ "Network Error: " ++  Text.unpack err      

