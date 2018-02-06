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

class <newmodule> a where
        toText :: a -> Text

instance <newmodule> MarketName where
        toText = T.toText 

instance <newmodule> Currency where
        toText = T.toText

instance FromJSON Ticker 

instance FromJSON Balance 
