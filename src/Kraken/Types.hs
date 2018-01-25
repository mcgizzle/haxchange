{-# LANGUAGE OverloadedStrings, GADTs #-}
module Kraken.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) ) 
import qualified Types as T

import Prelude as P
import Data.Monoid ((<>))
import Data.Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text 
import Data.Time
import Data.Time.ISO8601
import Data.Aeson
import Data.List (init)
import Data.HashMap.Lazy as HM
import Text.Read
import Network.Wreq (FormParam)

class Kraken a where
        toText :: a -> Text
        fromText :: Text -> a
        toAsset :: a -> Text

type Price = Text
type Volume = Text
type Order = String

type Params = [(Text,Text)]

data Opts = Opts {
                   optPath       :: String
                 , optParams     :: Params 
                 , optApiType    :: String
                 , optApiPubKey  :: String
                 , optApiPrivKey :: String
                 , optPost       :: Params 
                 , optInside     :: Bool
                 }

instance Kraken MarketName where
        toText (MarketName a b) = toText a <> toText b 

        fromText = T.fromText

instance Kraken Currency where
        toText (COIN BTC) = "XBT"
        toText a          = T.toText a

        fromText "XBT" = COIN BTC
        fromText a     = T.fromText a

        toAsset (COIN a) = "X" <> toText (COIN a)
        toAsset (FIAT a) = "Z" <> toText (FIAT a)

toPair :: MarketName -> Text
toPair (MarketName a b) = toAsset a <> toAsset b

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                bid <- o .: "b"
                ask <- o .: "a"
                last <- o .: "c"
                pure $ Ticker (t bid) (t ask) (t last) 
                        where t = read . head

instance FromJSON Balance where
        parseJSON = withObject "Balance" $ \o -> pure $ Balance $ toBal <$> HM.toList o
                where 
                        toBal :: (Text,Value) -> (Currency,Float)
                        toBal (cur,val) = (cur',val')
                                where cur' = fromText $ Text.tail cur
                                      val' = case fromJSON val of
                                               Error _ -> 0.00
                                               Success v -> read v 
