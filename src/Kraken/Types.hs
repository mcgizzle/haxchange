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
import qualified Data.Vector         as V
import           Prelude             as P

class KrakenText a where
        toText :: a -> Text
        toAsset :: a -> Text

instance KrakenText MarketName where
        toText = T.toText

        toAsset (MarketName a b) = toAsset a <> toAsset b

instance KrakenText Currency where
        toText (COIN BTC) = "XBT"
        toText a          = T.toText a

        toAsset (COIN a) = "X" <> toText (COIN a)
        toAsset (FIAT a) = "Z" <> toText (FIAT a)
        toAsset (NA a)   = "X" <> toText (NA a)

instance FromJSON OrderId where
        parseJSON (Object o) = parseObj $ HM.toList o
          where
            parseObj [(_, Object o')] = do
                    res <- o' .: "txid" <|> o' .: "order"
                    pure $ OrderId res
            parseObj _               = fail "More than one object was returned || Object not nested"
        parseJSON _ = fail "Object not received"

instance FromJSON Ticker where
        parseJSON (Object o) = parseObj $ HM.toList o
          where
            parseObj [(_,Object o')] = do
                  (bid:bidVol:_) <- o' .: "b"
                  (ask:askVol:_) <- o' .: "a"
                  pure $ Ticker (read bid) (read ask) (read askVol) (read bidVol)
            parseObj _ = fail "More than one object || Object not nested"
        parseJSON _ = fail "Object not received"

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
