{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Binance.Types where

import           Types               (Balance (..), Currency (..), Error (..),
                                      Market (..), Markets (..), OrderId (..),
                                      ServerTime (..), Ticker (..))
import qualified Types               as T

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Vector         as V
import           Prelude             as P

import           Debug.Trace

class BinanceText a where
        toText :: a -> Text

instance BinanceText Market where
        toText = T.toText

instance BinanceText Currency where
        toText = T.toText

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \ o -> do
                bid    <- o .: "bidPrice"
                ask    <- o .: "askPrice"
                askVolume <- o .: "askQty"
                bidVolume <- o .: "bidQty"
                pure $ Ticker (read bid) (read ask) (read askVolume) (read bidVolume)

instance FromJSON Market where
        parseJSON = withObject "Market" $ \o -> do
                mrkt <- o .: "symbol"
                let mrkt' = case (Text.length mrkt) - 6 of
                              0 -> mrkt
                              n -> Text.drop n mrkt
                let [f,s] = Text.chunksOf 3 mrkt'
                pure $ Market (T.fromText f) (T.fromText s)

instance FromJSON Markets where
        parseJSON = withArray "Markets" $ \a -> Markets <$> mapM parseJSON (V.toList a)

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
