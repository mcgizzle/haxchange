module Api where

import Types
import Internal
import Data.Text
import Data.Monoid

defaultOpts = Opts mempty mempty

getMarkets :: IO (Either String [Market])
getMarkets = runApi defaultOpts { optPath = "getmarkets"}

getTicker :: String -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts { optPath = "getticker", optParams = [(pack "market",pack mrkt)]}
