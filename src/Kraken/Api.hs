module Kraken.Api where

import Kraken.Types
import Kraken.Internal
import qualified Data.Text as Text
import Data.List
import Data.Monoid

defaultOpts = Opts mempty mempty "public"

getTicker :: String -> IO (Either String Ticker)
getTicker mrkt = runApi defaultOpts { optPath = "Ticker", optParams = [(Text.pack "pair",Text.pack $ delete '-' mrkt)]}
