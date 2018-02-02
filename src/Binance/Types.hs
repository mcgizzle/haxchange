{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module Binance.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) 
             , Opts(..) )
import qualified Types as T

import           Prelude as P
import           Data.Text (Text)
import qualified Data.Text as Text 
import           Data.Monoid ((<>))
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Network.Wreq (FormParam)
import           GHC.Generics

class Binance a where
        toText :: a -> Text

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

instance FromJSON Balance where
        parseJSON = withObject "Balance" $ \ o -> do
                bal <- o .: "balances" 
                curs <- bal .: "asset"
                amounts <- bal .: "free"
                pure $ Balance $ zip curs (read amounts)

instance FromJSON Currency

newtype ServerTime = SeverTime { serverTime :: Float }
        deriving (Show,Generic)
instance FromJSON ServerTime
