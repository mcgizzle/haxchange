{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Bittrex.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) ) 
import qualified Types as T

import Prelude as P
import Data.Maybe
import Data.Text as Text
import Data.Monoid ((<>))
import Data.Aeson
import Data.List.Split (splitOn)
import Text.Read

type Params = [(Text,Text)]

data Opts = Opts {
                   optPath    :: String
                 , optParams  :: Params 
                 , optApiType :: String
                 }

class Bittrex a where
        toText :: a -> Text

instance Bittrex MarketName where
        toText (MarketName a b) = toText a <> "-" <> toText b

instance Bittrex Currency where
        toText = T.toText

instance FromJSON MarketName where
        parseJSON = withText "MarketName" $ \t -> do
                let [t1,t2] = Text.splitOn "-" t
                pure $ MarketName (T.fromText t1) (T.fromText t2)

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                bid <- o .: "Bid"
                ask <- o .: "Ask"
                pure $ Ticker bid ask Nothing

