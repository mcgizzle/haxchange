{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Kraken.Types where

import           Types                (Balance (..), Currency (..),
                                       Currency' (..), Error (..), Market (..),
                                       Markets (..), OrderId (..),
                                       ServerTime (..), Ticker (..),
                                       Tickers (..))
import qualified Types                as T

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.Attoparsec.Text as Atto
import           Data.HashMap.Lazy    as HM
import           Data.List            (intersperse)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector          as V
import           Prelude              as P

class KrakenText a where
        toText :: a -> Text

instance KrakenText Markets where
        toText m = mconcat $ intersperse "," $ toText <$> unMarkets m

parseMarket :: Atto.Parser Market
parseMarket = Market <$> ((T.fromText . Text.tail <$> Atto.take 4) <|> (T.fromText . Text.tail <$> Atto.take 5))
                     <*> (T.fromText . Text.tail <$> ("XETH" <|> "XXBT" <|> "ZCAD" <|> "ZJPY"))

instance KrakenText Market where
        toText (Market a b) = toText a <> toText b

instance KrakenText Currency where
        toText (FIAT a) = "Z" <> toText a
        toText (COIN a) = "X" <> toText a
        toText a        = "X" <> T.toText a

instance KrakenText Currency' where
        toText BTC = "XBT"
        toText a   = T.toText a

instance FromJSON ServerTime where
        parseJSON = withObject "Time" $ \ o ->
                ServerTime <$> o .: "unixtime"

unParsedMarket :: Market
unParsedMarket = Market (NA "Failed Parse") (NA "Failed Parse")

instance FromJSON Markets where
        parseJSON = withObject "Markets" $ \o -> Markets . P.filter ((/=) unParsedMarket) <$> (toMarket . fst) `mapM` HM.toList o
          where
            toMarket :: Text -> Parser Market
            toMarket a = case Atto.parseOnly parseMarket a of
                           Left _  -> pure $ Market (T.fromText "Failed Parse") (T.fromText "Failed Parse")
                           Right r -> pure r

instance FromJSON Tickers where
        parseJSON (Object o) = Tickers <$> mapM parseObj (HM.toList o)
          where
            parseObj (pair,Object o') = do
                  (bid:bidVol:_) <- o' .: "b"
                  (ask:askVol:_) <- o' .: "a"
                  case Atto.parseOnly parseMarket pair of
                    Left _ -> fail "Error parsing market"
                    Right r -> pure $ Ticker r (read bid) (read ask) (read askVol) (read bidVol)
            parseObj _ = fail "Error parsing ticker"
        parseJSON _ = fail "Object not received"

instance FromJSON Balance where
        parseJSON = withObject "Balance" $ \o -> pure $ Balance $ Map.fromList $ toBal <$> HM.toList o
          where
            toBal :: (Text,Value) -> (Currency,Float)
            toBal (cur,val) = (cur',val')
              where cur' = T.fromText $ Text.tail cur
                    val' = case fromJSON val of
                             Error _   -> 0.00
                             Success v -> read v


instance FromJSON OrderId where
        parseJSON (Object o) = parseObj $ HM.toList o
          where
            parseObj [(_, Object o')] = do
                    res <- o' .: "txid" <|> o' .: "order"
                    pure $ OrderId res
            parseObj _               = fail "More than one object was returned || Object not nested"
        parseJSON _ = fail "Object not received"


instance FromJSON Error where
        parseJSON (Array a) = pure $ ExchangeError $ parseError <$> V.toList a
        parseJSON _         = pure $ UnknownError "NA"

parseError :: Value -> T.Err
parseError (String "EQuery:Unknown asset pair")  = T.UnknownAssetPair
parseError (String "EService:Unavailable")       = T.Unavailable
parseError (String "EGeneral:Invalid arguments") = T.InvalidArguments
parseError (String e)                            = T.Err e
parseError _                                     = T.Err "NA"
