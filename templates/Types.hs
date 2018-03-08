{-# LANGUAGE OverloadedStrings #-}
module <newmodule>.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) ) 
import qualified Types as T

import           Data.Text (Text)
import qualified Data.Text as Text 
import           Data.Aeson

class <newmodule>Text a where
        toText :: a -> Text

instance <newmodule>Text MarketName where
        toText = T.toText 

instance <newmodule>Text Currency where
        toText = T.toText

instance FromJSON Markets

instance FromJSON Market

instance FromJSON Tickers

instance FromJSON Ticker 

instance FromJSON Balance 

instance FromJSON OrderId

instance FromJSON ServerTime
