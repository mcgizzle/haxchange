{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kraken.Api where

import           Types           (Balance (..), Error (..), Market (..),
                                  Markets (..), Opts (..), Order (..), OrderId,
                                  ServerTime, Ticker (..))
import           Utils

import           Data.Text       (Text)
import           Kraken.Internal
import           Kraken.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

ping :: IO (Either Error ServerTime)
ping = runGetApi defaultOpts { optPath = "Time" }

getMarkets :: IO (Either Error Markets)
getMarkets = runGetApi defaultOpts { optPath = "AssetPairs" }

getTicker :: Market -> IO (Either Error Ticker)
getTicker mrkt = runGetApi defaultOpts
        { optPath = "Ticker"
        , optParams = [("pair",toAsset mrkt)]
        }

getBalance :: IO (Either Error Balance)
getBalance = withKeys "keys/kraken.txt" $ \ pubKey privKey ->
        runPostApi defaultOpts
                { optPath = "Balance"
                , optApiType = "private"
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey }

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
                , optApiPubKey = pubKey }

buyLimit :: Order -> IO (Either Error OrderId)
buyLimit = placeOrder "buy"

sellLimit :: Order -> IO (Either Error OrderId)
sellLimit = placeOrder "sell"

