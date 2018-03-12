{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Haxchange.Kraken.Api where

import           Types                     (APIKeys (..), Balance (..),
                                            Error (..), Markets (..), Opts (..),
                                            Order (..), OrderId, ServerTime,
                                            Tickers (..))

import           Data.Text                 (Text)
import           Haxchange.Kraken.Internal
import           Haxchange.Kraken.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

ping :: IO (Either Error ServerTime)
ping = runGetApi defaultOpts { optPath = "Time" }

getMarkets :: IO (Either Error Markets)
getMarkets = runGetApi defaultOpts { optPath = "AssetPairs" }

getTicker :: Markets -> IO (Either Error Tickers)
getTicker mrkts = runGetApi defaultOpts
        { optPath = "Ticker"
        , optParams = [("pair",toText mrkts)]
        }

getBalance :: APIKeys -> IO (Either Error Balance)
getBalance (APIKeys pubKey privKey) =
        runPostApi defaultOpts
                { optPath = "Balance"
                , optApiType = "private"
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey }

placeOrder :: APIKeys -> Text -> Order -> IO (Either Error OrderId)
placeOrder (APIKeys pubKey privKey) t Order{..} =
        runPostApi defaultOpts
                { optPath = "AddOrder"
                , optApiType = "private"
                , optPost = [ ("pair", toText orderMarket)
                            , ("type",t)
                            , ("ordertype","limit")
                            , ("price",orderPrice)
                            , ("volume",orderVolume)
                            , ("validate","true")
                            ]
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey }

buyLimit :: APIKeys -> Order -> IO (Either Error OrderId)
buyLimit keys = placeOrder keys "buy"

sellLimit :: APIKeys -> Order -> IO (Either Error OrderId)
sellLimit keys = placeOrder keys "sell"

