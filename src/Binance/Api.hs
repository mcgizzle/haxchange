{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Binance.Api where

import           Types            (Balance, Error, Market, Markets (..),
                                   Opts (..), Order (..), OrderId, ServerTime,
                                   Ticker)
import           Utils

import           Binance.Internal
import           Binance.Types
import           Data.Text        (Text)
import qualified Data.Text        as Text

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public" "v1" mempty mempty mempty

ping :: IO (Either Error ServerTime)
ping = runGetApi defaultOpts { optPath = "time"}

getMarkets :: IO (Either Error Markets)
getMarkets = runGetApi defaultOpts
        { optApiVersion = "v1"
        , optPath       = "ticker/allBookTickers"
        }

getTicker :: Market -> IO (Either Error Ticker)
getTicker mrkt = runGetApi defaultOpts
        { optApiVersion = "v3"
        , optPath       = "ticker/bookTicker"
        , optParams     = [("symbol",toText mrkt)]
        }

getBalance :: IO (Either Error Balance)
getBalance = withKeys "keys/binance.txt" $ \ pubKey privKey -> do
        t <- timeInMilli
        runGetPrivApi defaultOpts
                { optPath = "account"
                , optApiVersion = "v3"
                , optApiPubKey = pubKey
                , optApiPrivKey = privKey
                , optParams = [ ("timestamp",Text.pack t)]
                }

placeOrder :: Text -> Order -> IO (Either Error OrderId)
placeOrder side Order{..} = withKeys "keys/binance.txt" $ \ pubKey privKey -> do
        t <- timeInMilli
        runPostApi defaultOpts
                    {
                      optPath = "order/test"
                    , optApiVersion = "v3"
                    , optApiPubKey = pubKey
                    , optApiPrivKey = privKey
                    , optPost = [ ("symbol", toText orderMarket)
                                , ("type", "limit")
                                , ("side", side )
                                , ("quantity", orderVolume )
                                , ("price", orderPrice )
                                , ("timeInForce", "GTC")
                                , ("timestamp",Text.pack t) ]
                    }

buyLimit :: Order -> IO (Either Error OrderId)
buyLimit = placeOrder "buy"

sellLimit :: Order -> IO (Either Error OrderId)
sellLimit = placeOrder "sell"
