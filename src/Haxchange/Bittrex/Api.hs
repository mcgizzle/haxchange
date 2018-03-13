{-# LANGUAGE OverloadedStrings #-}
module Haxchange.Bittrex.Api where

import           Haxchange.Types                      (Market (..), Ticker (..))

import           Haxchange.Bittrex.Internal
import           Haxchange.Bittrex.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public"

getTicker :: Market -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts
        { optPath = "getticker"
        , optParams = [("market",toText mrkt)] }
