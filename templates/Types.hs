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

import Prelude as P
import Data.Text (Text)
import qualified Data.Text as Text 
import Data.Monoid ((<>))
import Data.Aeson
import Network.Wreq (FormParam)
import GHC.Generics

class <newmodule> a where
        toText :: a -> Text

type Params = [(Text,Text)]

data Opts = Opts {
                   optPath       :: String
                 , optParams     :: Params 
                 , optApiType    :: String
                 , optApiPubKey  :: String
                 , optApiPrivKey :: String
                 , optPost       :: Params 
                 }

instance <newmodule> MarketName where
        toText = T.toText 

instance <newmodule> Currency where
        toText = T.toText

instance FromJSON Ticker 

instance FromJSON Balance 
