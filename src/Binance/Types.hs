{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Binance.Types where

import           Types            (Balance (..), Currency (..), MarketName (..),
                                   Ticker (..))
import qualified Types            as T

import           Data.Aeson
import           Data.Aeson.Types (Parser (..))
import           Data.Text        (Text)
import qualified Data.Vector      as V
import           GHC.Generics
import           Prelude          as P


class Binance a where
        toText :: a -> Text

instance Binance MarketName where
        toText = T.toText

instance Binance Currency where
        toText = T.toText

instance FromJSON Ticker where
        parseJSON = withObject "Ticker" $ \ o -> do
                bid    <- o .: "bidPrice"
                ask    <- o .: "askPrice"
                volume <- o .: "askQty"
                pure $ Ticker (read bid) (read ask) (Just $ read volume)

instance FromJSON Balance where
        parseJSON = withObject "Balance" $ \ o -> do
                bal <- o .: "balances"
                Balance . filter (\(_,y) -> y /= 0) <$> mapM toBal (V.toList bal)
                        where
                                toBal :: Value -> Parser (Currency,Float)
                                toBal = withObject "O" $ \ o -> do
                                        cur <- o .: "asset"
                                        amount <- o .: "free"
                                        pure (T.fromText cur,read amount)


instance FromJSON Currency

newtype ServerTime = SeverTime { serverTime :: Float }
        deriving (Show,Generic)
instance FromJSON ServerTime

newtype OrderResponse = OrderResponse { orderId :: Int }
        deriving (Show)
instance FromJSON OrderResponse where
        parseJSON = withObject "OrderRes" $ \ o ->do
                pure $ OrderResponse 1
