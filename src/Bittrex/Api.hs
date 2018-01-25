{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Api where

import Types

import Bittrex.Types
import Bittrex.Internal
import Data.Text as Text
import Data.Monoid

defaultOpts = Opts mempty mempty "public"

getMarkets :: IO (Either String [Market])
getMarkets = runApi defaultOpts 
        { optPath = "getmarkets"}

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts 
        { optPath = "getticker"
        , optParams = [("market",Types.toText mrkt)] }
