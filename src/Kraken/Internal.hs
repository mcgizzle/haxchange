{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kraken.Internal where

import           Types                  (Opts (..))
import           Utils

import           Control.Exception      as E
import           Control.Lens
import           Crypto.Hash.SHA256     as SHA256
import           Crypto.Hash.SHA512     as SHA512
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as Byte
import           Data.List              (intercalate)
import           Data.Monoid
import           Network.HTTP.Client    (HttpException)
import           Network.Wreq

-- HELPER FUNCTIONS ---------------------------------------------------------------------------
getUrl :: Opts -> ([String] -> [String]) -> String
getUrl Opts{..} f = intercalate "/" $ f [ "https://api.kraken.com"
                                        , "0"
                                        , optApiType
                                        , optPath ]

getUri :: Opts -> String
getUri opts = getUrl opts (\ (_:xs) -> "":xs)

apiSign :: Opts -> String -> ByteString
apiSign opts@Opts{..} nonce = B64.encode $ SHA512.hmac b64Api
                                         $ uri <> nonceAndPost
       where uri = Byte.pack $ getUri opts
             nonceAndPost = SHA256.hash $ Byte.pack nonce <> Byte.pack ("nonce="++nonce)
             Right b64Api = B64.decode optApiPrivKey

-- HEADERS -----------------------------------------------------------------------------------
getDefaults :: Opts -> Network.Wreq.Options
getDefaults Opts{..} = defaults & header "Accept" .~ ["application/json"]
                                & params .~  optParams

postDefaults :: String -> Opts -> Network.Wreq.Options
postDefaults nonce opts@Opts{..} = getDefaults opts & header "API-Key" .~ [optApiPubKey]
                                                    & header "API-Sign" .~ [apiSign opts nonce]
                                                    & header "Content-Type" .~ ["application/x-www-form-urlencoded"]

-- API CALLS ----------------------------------------------------------------------------------
runGetApi :: FromJSON r => Opts -> Bool -> IO (Either String r)
runGetApi opts@Opts{..} b = do
        let opts' = getDefaults opts
            url = getUrl opts id
        (getWith opts' url >>= handleRes b) `E.catch` handleExcept

runPostApi :: FromJSON r => Opts -> Bool -> IO (Either String r)
runPostApi opts@Opts{..} b = do
        nonce <- getNonce
        let opts' = postDefaults nonce opts
            url = getUrl opts id
            body = [ "nonce" := nonce ] <> toFormParam optPost
        (postWith opts' url body >>= handleRes b) `E.catch` handleExcept

-- HANDLER ----------------------------------------------------------------------------------
handleExcept :: FromJSON r => HttpException -> IO (Either String r)
handleExcept e = return $ Left $ "Network Exception: " ++ show e

handleRes :: (Show a, FromJSON b, AsValue a) => Bool -> Response a -> IO (Either String b)
handleRes member res = do
        let err = res ^. responseBody . key "error" . _Array
        let p = if member then res ^? responseBody . key "result" . members
                          else res ^? responseBody . key "result"
        case p of
          Just p' -> case fromJSON p' of
                          Success s -> return $ Right s
                          Error e   -> return $ Left $ "Parse Error: " ++ e
          Nothing -> return $ Left $ "Network Error: " ++ show err

