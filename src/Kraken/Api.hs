module Kraken.Api where

import Types
import Kraken.Types
import Kraken.Internal
import qualified Data.Text as Text
import Data.List
import Data.Monoid

defaultOpts = Opts mempty mempty "public" mempty mempty mempty 

getKeys :: IO [String]
getKeys = lines <$> readFile "keys.txt"

getTicker :: String -> IO (Either String Ticker)
getTicker mrkt = runGetApi defaultOpts 
        { optPath = "Ticker"
        , optParams = [(Text.pack "pair",Text.pack $ delete '-' mrkt)] }

getBalance :: IO (Either String Balance)
getBalance = withKeys $ \ pubKey privKey -> 
        runGetApi defaultOpts 
                { optPath = "Balance"
                , optApiType = "private"
                , optApiPrivKey = privKey
                , optApiPubKey = pubKey }

placeBuyLimit :: Price -> Volume -> IO (Either String Order)
placeBuyLimit p v = withKeys $ \ pubKey privKey ->
        runPostApi defaultOpts 
                { optPath = "AddOrder"
                , optPost = [ ("type","buy")
                            , ("price",p)
                            , ("volume",v)
                            , ("validate","true") ] }

withKeys :: (String -> String -> IO b) -> IO b
withKeys f = do
        [pubKey,privKey] <- getKeys
        f pubKey privKey
