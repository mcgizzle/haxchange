{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kraken.Internal where

import           Kraken.Types           ()
import           Types                  (Error (..), Opts (..))
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
import           Data.Text              (pack)
import           Network.Wreq

-- HELPER FUNCTIONS ---------------------------------------------------------------------------
getUrl :: Opts -> ([String] -> [String]) -> String
getUrl Opts{..} f = intercalate "/" $ f [ "https://api.kraken.com"
                                        , "0"
                                        , optApiType
                                        , optPath ]

getUri :: Opts -> String
getUri opts = getUrl opts (\(_:xs) -> "":xs)

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
runGetApi :: FromJSON j => Opts -> Bool -> IO (Either Error j)
runGetApi opts@Opts{..} b = do
        let opts' = getDefaults opts
            url = getUrl opts id
        (getWith opts' url >>= handleRes b) `E.catch` handleExcept

runPostApi :: FromJSON j => Opts -> Bool -> IO (Either Error j)
runPostApi opts@Opts{..} b = do
        nonce <- getNonce
        let opts' = postDefaults nonce opts
            url = getUrl opts id
            body = [ "nonce" := nonce ] <> toFormParam optPost
        (postWith opts' url body >>= handleRes b) `E.catch` handleExcept

-- HANDLER ----------------------------------------------------------------------------------
handleRes :: (Show a, FromJSON j, AsValue a) => Bool -> Response a -> IO (Either Error j)
handleRes member resp = do
        let (Just err) = resp ^? responseBody . key "error"
        let result = if member then resp ^? responseBody . key "result" . members
                               else resp ^? responseBody . key "result"
        case result of
            Just r  -> case fromJSON r of
                         Success s -> return $ Right s
                         Error e   -> return $ Left  $ ParseError $ show e
            Nothing -> case fromJSON err of
                         Success s -> return $ Left s
                         Error e   -> return $ Left $ UnknownError e
