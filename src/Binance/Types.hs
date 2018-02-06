{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module Binance.Types where

import Debug.Trace

import Types ( Api
             , Ticker(..)
             , Currency(..)
             , Currency'(..)
             , MarketName(..)
             , Balance(..) 
             , Opts(..) )
import qualified Types as T

import           Prelude as P
import           Data.Text (Text)
import qualified Data.Text as Text 
import           Data.Monoid ((<>))
import           Data.Aeson
import           Data.Aeson.Types (Parser(..),Array)
import           Data.ByteString (ByteString)
import           GHC.Generics
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V


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
