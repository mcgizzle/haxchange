{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
module Kraken.Internal where

import Kraken.Types

import Util
import Control.Arrow (first)
import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson
import Data.Monoid
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as Byte
import Data.ByteString.Base64 as B64
import "cryptohash-sha512" Crypto.Hash.SHA512 (hmac)
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
        let body = [ "nonce" := nonce ] <> body' 
            --body' = mconcat (fmap (\(x,y) -> [Byte.pack x := Byte.pack y]) optPost)
            body' = unzipWith (:=) $ fmap (first Byte.pack) optPost
        let url = intercalate "/" [ "https://api.kraken.com/0"
                                  , optApiType
                                  , optPath ]
        let apisign = B64.encode $ hmac b64Api (uri <> nonceAndPost)
            uri = Byte.pack $ "/0/" ++ optApiType ++ "/" ++ optPath
            nonceAndPost = SHA256.hash $ Byte.pack nonce <> Byte.pack ("nonce="++nonce)
            Right b64Api = B64.decode $ Byte.pack optApiPrivKey
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


