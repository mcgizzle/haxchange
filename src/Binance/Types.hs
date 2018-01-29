{-# LANGUAGE OverloadedStrings #-}
module Binance.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) ) 
import qualified Types as T

import           Prelude as P
import           Data.Text (Text)
import qualified Data.Text as Text 
import           Data.Monoid ((<>))
import           Data.Aeson
import           Network.Wreq (FormParam)
import           GHC.Generics

class Binance a where
        toText :: a -> Text

type Params = [(Text,Text)]

data Opts = Opts {
                   optPath       :: String
                 , optParams     :: Params 
                 , optApiVersion :: String
                 , optApiPubKey  :: String
                 , optApiPrivKey :: String
                 , optPost       :: Params 
                 }

instance Binance MarketName where
        toText = T.toText 

instance Binance Currency where
        toText = T.toText

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \ o -> do
                bid    <- o .: "bidPrice"
                ask    <- o .: "askPrice"
                volume <- o .: "askQty"
                pure $ Ticker (read bid) (read ask) (Just $ read volume)

instance FromJSON Balance 

instance FromJSON Currency
