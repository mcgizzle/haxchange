{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module <newmodule>.Api where

import Types 
        ( Api
        , Ticker(..)
        , Currency(..)
        , Currency'(..)
        , MarketName(..)
        , Balance(..) 
        , Order(..)) 
import qualified Types as T

import <newmodule>.Types
import <newmodule>.Internal
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List
import Data.Monoid

defaultOpts = Opts mempty mempty "public" mempty mempty mempty

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = return $ Left "Implement Me!"

getBalance :: IO (Either String Balance)
getBalance = withKeys $ \ pubKey privKey -> return $ Left "Implement Me!" 

buyLimit :: Order -> IO (Either String Order)
buyLimit Order{..} = return $ Left "Implement Me!"

sellLimit :: Order -> IO (Either String Order)
sellLimit Order{..} = return $ Left "Implement Me!"

------ KEYS --------------------------------
getKeys :: IO [String]
getKeys = lines <$> readFile "keys.txt"

withKeys :: (String -> String -> IO b) -> IO b
withKeys f = do
        [pubKey,privKey] <- getKeys
        f pubKey privKey
