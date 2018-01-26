{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Api where

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) ) 
import qualified Types as T

import Bittrex.Types
import Bittrex.Internal
import Data.Text as Text
import Data.Monoid

defaultOpts = Opts mempty mempty "public"

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts 
        { optPath = "getticker"
        , optParams = [("market",toText mrkt)] }
