{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Binance.Api where

import Utils
import Types 
        ( Api
        , Ticker(..)
        , Currency(..)
        , Currency'(..)
        , MarketName(..)
        , Balance(..) 
        , Order(..)
        , Opts(..)
        , Params )
import qualified Types as T

import           Binance.Types
import           Binance.Internal
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.List
import           Data.Monoid
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Time.Clock.POSIX
import           Data.Time.Clock

defaultOpts = Opts mempty mempty "public" "v1" mempty mempty mempty

ping :: IO (Either String ServerTime)
ping = runGetApi defaultOpts { optPath = "time"}

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = runGetApi defaultOpts 
        { optApiVersion = "v3"
        , optPath       = "ticker/bookTicker"
        , optParams     = [("symbol",toText mrkt)]
        }

getBalance :: IO (Either String Balance)
getBalance = withKeys $ \ pubKey privKey -> do
        t <- getNonce 
        runGetPrivApi defaultOpts 
                { optPath = "account"
                , optApiVersion = "v3"
                , optApiPubKey = pubKey
                , optApiPrivKey = privKey
                , optParams = [ ("timestamp",Text.pack $ show $ (read t :: Integer) * 1000)]
                }

buyLimit :: Order -> IO (Either String Order)
buyLimit Order{..} = return $ Left "Implement Me!"

sellLimit :: Order -> IO (Either String Order)
sellLimit Order{..} = return $ Left "Implement Me!"

--------------- KEYS ----------------------------------------------
getKeys :: IO [ByteString]
getKeys = B8.lines <$> B8.readFile "keys/binance.txt"
withKeys :: (ByteString -> ByteString -> IO b) -> IO b
withKeys f = do
        [pubKey,privKey] <- getKeys
        f pubKey privKey
