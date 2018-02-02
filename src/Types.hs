{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module Types where

import           Prelude as P
import           Data.Aeson
import           Data.Monoid ((<>),mconcat)
import           Data.Map as Map
import           Data.Text (Text)
import           Data.List.Split (splitOn)
import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import           GHC.Generics

-------------------------------------
type Params = [(Text,Text)]

data Opts = 
        Opts {
               optPath       :: String
             , optParams     :: Params 
             , optApiType    :: String
             , optApiVersion :: String
             , optApiPubKey  :: ByteString
             , optApiPrivKey :: ByteString
             , optPost       :: Params 
             }

-------------------------------------

class Api a where
        fromText :: Text -> a
        toText :: a -> Text

data Currency = 
        FIAT Currency' 
      | COIN Currency' 
      | NA Text
 deriving(Eq,Show,Read,Generic)

instance Api Currency where
        toText (FIAT a) = toText a
        toText (COIN a) = toText a

        fromText "EUR" = FIAT EUR
        fromText "BTC" = COIN BTC
        fromText "XBT" = COIN BTC
        fromText "XRP" = COIN XRP
        fromText "ETH" = COIN ETH
        fromText "LTC" = COIN LTC
        fromText a     = NA a 


data Currency' = 
        EUR 
      | BTC 
      | XRP 
      | ETH
      | LTC
      | Text
        deriving(Show,Eq,Read,Generic) 

instance FromJSON Currency'

instance Api Currency' where 
        toText EUR = "EUR"
        toText BTC = "BTC"
        toText XRP = "XRP"
        toText ETH = "ETH"
        toText LTC = "LTC"

        fromText "EUR" = EUR
        fromText "BTC" = BTC
        fromText "XBT" = BTC
        fromText "XRP" = XRP
        fromText "ETH" = ETH
        fromText "LTC" = LTC


data MarketName = MarketName Currency Currency
        deriving (Show,Eq,Read)

instance Api MarketName where
        toText (MarketName a b) = toText a <> toText b

        fromText a = MarketName (fromText $ head s) (fromText $ P.last s) 
                where s = Text.splitOn "-" a

newtype Balance = Balance [(Currency,Float)]
        deriving(Show,Generic)

data Ticker = 
        Ticker {
                 tickerBid    :: Float
               , tickerAsk    :: Float
               , tickerVolume :: Maybe Float
               }
         deriving (Eq,Show,Generic) 

data Order = 
        Order {
                orderMarket :: MarketName
              , orderPrice  :: Text
              , orderVolume :: Text
              }
        deriving(Eq,Show,Generic)
