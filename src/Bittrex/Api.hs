{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Api where

import           Types            (MarketName (..), Ticker (..))

import           Bittrex.Internal
import           Bittrex.Types

defaultOpts = Opts mempty mempty "public"

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts
        { optPath = "getticker"
        , optParams = [("market",toText mrkt)] }
