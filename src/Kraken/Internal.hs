{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
module Kraken.Internal where

import Debug.Trace
--------------------------------------------------------------------------------------------------
import Types (Opts(..))
import Utils
import Kraken.Types
--------------------------------------------------------------------------------------------------
import           Util
import           Control.Arrow (first)
import           Network.Wreq
import           Control.Lens 
import           Data.Aeson.Lens 
import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as Byte
import           Data.ByteString.Base64 as B64
import           Crypto.Hash.SHA512 as SHA512 
import           Crypto.Hash.SHA256 as SHA256
----------------------------------------------------------------------------------------------------

getUrl :: Opts -> ([String] -> [String]) -> String
getUrl Opts{..} f = intercalate "/" $ f [ "https://api.kraken.com"
                                        , "0"
                                        , optApiType
                                        , optPath ]

getUri :: Opts -> String 
getUri opts = getUrl opts (\ (x:xs) -> "":xs)

----------------------------------------------------------------------------------------------------

runGetApi :: FromJSON r => Opts -> Bool -> IO (Either String r)
runGetApi opts@Opts{..} b = do
        let opts' = defaults & header "Accept" .~ ["application/json"] 
                             & params .~  optParams
            url = getUrl opts id
        getWith opts' url >>= handleRes b

runPostApi :: FromJSON r => Opts -> Bool -> IO (Either String r)
runPostApi opts@Opts{..} b = do
        nonce <- getNonce
        let body = [ "nonce" := nonce ] <> toFormParam optPost 
            url = getUrl opts id
            apisign = B64.encode $ SHA512.hmac b64Api (uri <> nonceAndPost)
            uri = Byte.pack $ getUri opts
            nonceAndPost = SHA256.hash $ Byte.pack nonce <> Byte.pack ("nonce="++nonce) 
            Right b64Api = B64.decode optApiPrivKey
            opts' = defaults & header "API-Key" .~ [optApiPubKey] 
                             & header "API-Sign" .~ [apisign]
                             & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
                             & header "Accept" .~ ["application/json"] 
        postWith opts' url body >>= handleRes b

handleRes :: (Show a, FromJSON b, AsValue a) => Bool -> Response a -> IO (Either String b)
handleRes member res = do
        let err = res ^. responseBody . key "error" . _Array
        let p = if member then res ^? responseBody . key "result" . members
                          else res ^? responseBody . key "result" 
        case p of
          Just p' -> case fromJSON p' of
                          Success s -> return $ Right s
                          Error e -> return $ Left $ "Parse Error: " ++ e
          Nothing -> return $ Left $ "Network Error: " ++ show err      

