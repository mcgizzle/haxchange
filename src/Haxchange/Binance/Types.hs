{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haxchange.Binance.Types where

import           Haxchange.Types                (Balance (..), Currency (..), Error (..),
                                       Market (..), Markets (..), OrderId (..),
                                       ServerTime (..), Ticker (..),
                                       Tickers (..))
import qualified Haxchange.Types                as T
import           Haxchange.Utils

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.Attoparsec.Text as Atto
import           Data.List            (intersperse)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Vector          as V
import           Prelude              as P

import           Debug.Trace

class ToText a where
        toText :: a -> Text

instance ToText Markets where
        toText m = mconcat $ intersperse "," $ toText <$> unMarkets m

parseMarket :: Atto.Parser Market
parseMarket = Market <$> ((T.fromText <$> Atto.take 3) <|> (T.fromText <$> Atto.take 4))
                     <*> (T.fromText <$> ("BTC" <|> "ETH" <|> "BNB" <|> "USDT"))

instance ToText Market where
        toText = T.toText

instance ToText Currency where
        toText = T.toText

instance FromJSON Tickers where
        parseJSON (Array a) = Tickers <$> mapM parseJSON (V.toList a)
        parseJSON o         = Tickers . pure <$> parseJSON o

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \ o -> do
                mrkt <- o .: "symbol"
                bid    <- o .: "bidPrice"
                ask    <- o .: "askPrice"
                askVolume <- o .: "askQty"
                bidVolume <- o .: "bidQty"
                parsedMrkt <- monoidParse parseMarket mrkt
                pure $ Ticker parsedMrkt (read bid) (read ask) (read askVolume) (read bidVolume)

instance FromJSON Market where
        parseJSON = withObject "Market" $ \o ->  do
                mrkt <- o .: "symbol"
                monoidParse parseMarket mrkt

instance FromJSON Markets where
        parseJSON = withArray "Markets" $ \a -> Markets . P.filter (mempty /=) <$> mapM parseJSON (V.toList a)

instance FromJSON Balance where
        parseJSON = withObject "Account" $ \ o -> do
                bal <- o .: "balances"
                Balance . Map.fromList . filter ((/=) 0 . snd) <$> mapM toBal (V.toList bal)
                        where
                                toBal :: Value -> Parser (Currency,Float)
                                toBal = withObject "Balances" $ \ o -> do
                                        cur <- o .: "asset"
                                        amount <- o .: "free"
                                        pure (T.fromText cur,read amount)


instance FromJSON Currency

instance FromJSON ServerTime where
        parseJSON = withObject "ServerTime" $ \o ->
                ServerTime <$> o .: "serverTime"

instance FromJSON OrderId where
        parseJSON = withObject "OrderId" $ \ o ->do
                oId <- o .: "orderId" <|> pure "TEST"
                pure $ OrderId oId

instance FromJSON Error where
        parseJSON = withObject "Error" $ \ o -> do
                msg <- o .: "msg"
                pure $ parseError msg

parseError :: String -> Error
parseError t = traceShow t $ UnknownError t
