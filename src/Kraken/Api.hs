module Kraken.Api where

import Types
import Kraken.Types
import Kraken.Internal
import qualified Data.Text as Text
import Data.List
import Data.Monoid

defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

getKeys :: IO [String]
getKeys = lines <$> readFile "keys.txt"

getTicker :: String -> IO (Either String Ticker)
getTicker mrkt = get defaultOpts { optPath = "Ticker", optParams = [(Text.pack "pair",Text.pack $ delete '-' mrkt)]}

getBalance :: IO (Either String Ticker)
getBalance = do 
        [pubKey,privKey] <- getKeys 
        post defaultOpts { optPath = "Balance"
                         , optApiType = "private"
                         , optApiPrivKey = privKey
                         , optApiPubKey = pubKey
                         }
