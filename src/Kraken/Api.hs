{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kraken.Api where

import           Types           (Balance (..), Error (..), MarketName (..),
                                  Opts (..), Order (..), OrderId, ServerTime,
                                  Ticker (..))
import           Utils

import           Data.Text       (Text)
import           Kraken.Internal
import           Kraken.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

ping :: IO (Either Error ServerTime)
ping = runGetApi defaultOpts { optPath = "Time" } False

getTicker :: MarketName -> IO (Either Error Ticker)
getTicker mrkt = runGetApi defaultOpts
        { optPath = "Ticker"
        , optParams = [("pair",toText mrkt)]
        } True

getBalance :: IO (Either Error Balance)
getBalance = withKeys "keys/kraken.txt" $ \ pubKey privKey ->
        runPostApi defaultOpts
                { optPath = "Balance"
                , optApiType = "private"
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey } False

placeOrder :: Text -> Order -> IO (Either Error OrderId)
placeOrder t Order{..} = withKeys "keys/kraken.txt" $ \ pubKey privKey ->
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

