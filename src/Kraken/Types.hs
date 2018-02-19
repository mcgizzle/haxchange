{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Kraken.Types where

import           Types             (Balance (..), Currency (..), Currency' (..),
                                    MarketName (..), Ticker (..))
import qualified Types             as T

import           Data.Aeson
import           Data.HashMap.Lazy as HM
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           GHC.Generics
import           Prelude           as P

class Kraken a where
        toText :: a -> Text
        toAsset :: a -> Text

newtype OrderResponse = OrderResponse { order :: Text }
        deriving(Generic,Show)

instance FromJSON OrderResponse

instance Kraken MarketName where
        toText = T.toText

        toAsset (MarketName a b) = toAsset a <> toAsset b

instance Kraken Currency where
        toText (COIN BTC) = "XBT"
        toText a          = T.toText a

        toAsset (COIN a) = "X" <> toText (COIN a)
        toAsset (FIAT a) = "Z" <> toText (FIAT a)
        toAsset (NA a)   = "X" <> toText (NA a)


instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \o -> do
                (bid:bidVol:_) <- o .: "b"
                (ask:askVol:_) <- o .: "a"
                pure $ Ticker (read bid) (read ask) (read askVol) (read bidVol)

instance FromJSON Balance where
        parseJSON = withObject "Balance" $ \o -> pure $ Balance $ toBal <$> HM.toList o
                where
                        toBal :: (Text,Value) -> (Currency,Float)
                        toBal (cur,val) = (cur',val')
                                where cur' = T.fromText $ Text.tail cur
                                      val' = case fromJSON val of
                                               Error _   -> 0.00
                                               Success v -> read v
