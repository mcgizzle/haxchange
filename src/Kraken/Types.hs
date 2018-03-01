{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Kraken.Types where

import           Types               (Balance (..), Currency (..),
                                      Currency' (..), Error (..),
                                      MarketName (..), OrderId (..),
                                      ServerTime (..), Ticker (..))
import qualified Types               as T

import           Control.Applicative
import           Data.Aeson
import           Data.HashMap.Lazy   as HM
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Vector         as V
import           Prelude             as P

class Kraken a where
        toText :: a -> Text
        toAsset :: a -> Text

instance FromJSON OrderId where
        parseJSON = withObject "OrderId" $ \ o -> do
                txids <- o .: "txid" <|> o .: "order"
                pure $ OrderId txids


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
instance FromJSON ServerTime where
        parseJSON = withObject "Time" $ \ o ->
                ServerTime <$> o .: "unixtime"


instance FromJSON Error where
        parseJSON (Array a) = pure $ ExchangeError $ parseError <$> V.toList a
        parseJSON _         = pure $ UnknownError "NA"

parseError :: Value -> T.Err
parseError (String "EQuery:Unknown asset pair")  = T.UnknownAssetPair
parseError (String "EService:Unavailable")       = T.Unavailable
parseError (String "EGeneral:Invalid arguments") = T.InvalidArguments
parseError (String e)                            = T.Err e
parseError _                                     = T.Err "NA"
