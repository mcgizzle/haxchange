{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Api where

import           Types            (Market (..), Ticker (..))

import           Bittrex.Internal
import           Bittrex.Types

defaultOpts :: Opts
defaultOpts = Opts mempty mempty "public"

getTicker :: Market -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts
        { optPath = "getticker"
        , optParams = [("market",toText mrkt)] }
