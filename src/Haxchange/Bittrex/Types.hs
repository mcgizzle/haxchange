{-# LANGUAGE OverloadedStrings #-}
module Haxchange.Bittrex.Types where

import           Haxchange.Types       (Currency (..), Market (..), Ticker (..))
import qualified Haxchange.Types       as T

import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Text   as Text
import           Prelude     as P

type Params = [(Text,Text)]

data Opts = Opts
  {
    optPath    :: String
  , optParams  :: Params
  , optApiType :: String
  }

class BittrexText a where
        toText :: a -> Text

instance BittrexText Market where
        toText (Market a b) = toText a <> "-" <> toText b

instance BittrexText Currency where
        toText = T.toText

instance FromJSON Market where
        parseJSON = withText "Market" $ \t -> do
                case Text.splitOn "-" t of
                  [t1,t2] -> pure $ Market (T.fromText t1) (T.fromText t2)
                  _       -> fail "Error parsing market name"

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                buy <- o .: "buy"
                bid <- buy .: "Rate"
                bidVol <- buy .: "Quantity"
                sell <- o .: "sell"
                ask <- sell .: "Quantity"
                askVol <- sell .: "Rate"
                pure $ Ticker MarketNA bid ask askVol bidVol

