{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Haxchange.Kraken.Types where

import           Haxchange.Types                (Balance (..), Currency (..),
                                       Currency' (..), Error (..), Market (..),
                                       Markets (..), OrderId (..),
                                       ServerTime (..), Ticker (..),
                                       Tickers (..))
import qualified Haxchange.Types                as T
import           Haxchange.Utils

import           Control.Applicative
import           Data.Aeson
import qualified Data.Attoparsec.Text as Atto
import           Data.HashMap.Lazy    as HM
import           Data.List            (intersperse)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Vector          as V
import           Prelude              as P

class ToText a where
        toText :: a -> Text

instance ToText Markets where
        toText m = mconcat $ intersperse "," $ toText <$> unMarkets m

parseMarket :: Atto.Parser Market
parseMarket = Market <$> ((T.fromText . Text.tail <$> Atto.take 4) <|> (T.fromText . Text.tail <$> Atto.take 5))
                     <*> (T.fromText . Text.tail <$> ( "ZEUR" <|> "XETH" <|> "XXBT" <|> "ZCAD" <|> "ZJPY"))
                     <* Atto.endOfInput

instance ToText Market where
        toText (Market a b) = toText a <> toText b
        toText MarketNA     = "ERROR"

instance ToText Currency where
        toText (FIAT a) = "Z" <> toText a
        toText (COIN a) = "X" <> toText a
        toText a        = "X" <> T.toText a

instance ToText Currency' where
        toText BTC = "XBT"
        toText a   = T.toText a

instance FromJSON ServerTime where
        parseJSON = withObject "Time" $ \ o ->
                ServerTime <$> o .: "unixtime"

instance FromJSON Markets where
        parseJSON = withObject "Markets" $ \o -> Markets . P.filter (mempty /=) <$> (monoidParse parseMarket . fst) `mapM` HM.toList o

instance FromJSON Tickers where
        parseJSON (Object o) = Tickers <$> mapM parseObj (HM.toList o)
          where
            parseObj (pair, Object o') = do
                  (bid:bidVol:_) <- o' .: "b"
                  (ask:askVol:_) <- o' .: "a"
                  mrkt <- attoAeson parseMarket pair
                  pure $ Ticker mrkt (read bid) (read ask) (read askVol) (read bidVol)
            parseObj _ = fail "Ticker parse fail"
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
