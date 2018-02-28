{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kraken.Api where

import           Types                 (Balance (..), Error (..),
                                        MarketName (..), Opts (..), Order (..),
                                        OrderId, Ticker (..))

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Text             (Text)
import           Kraken.Internal
import           Kraken.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

getTicker :: MarketName -> IO (Either Error Ticker)
getTicker mrkt = runGetApi defaultOpts
        { optPath = "Ticker"
        , optParams = [("pair",toText mrkt)]
        } True

getBalance :: IO (Either Error Balance)
getBalance = withKeys $ \ pubKey privKey ->
        runPostApi defaultOpts
                { optPath = "Balance"
                , optApiType = "private"
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey } False

placeOrder :: Text -> Order -> IO (Either Error OrderId)
placeOrder t Order{..} = withKeys $ \ pubKey privKey ->
        runPostApi defaultOpts
                { optPath = "AddOrder"
                , optApiType = "private"
                , optPost = [ ("pair", toAsset orderMarket)
                            , ("type",t)
                            , ("ordertype","limit")
                            , ("price",orderPrice)
                            , ("volume",orderVolume)
                            , ("validate","true")
                            ]
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey } True

buyLimit :: Order -> IO (Either Error OrderId)
buyLimit = placeOrder "buy"

sellLimit :: Order -> IO (Either Error OrderId)
sellLimit = placeOrder "sell"

--------------- KEYS ----------------------------------------------
getKeys :: IO [ByteString]
getKeys = B8.lines <$> B8.readFile "keys/kraken.txt"
withKeys :: (ByteString -> ByteString -> IO b) -> IO b
withKeys f = do
        [pubKey,privKey] <- getKeys
        f pubKey privKey
