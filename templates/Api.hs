{-# LANGUAGE OverloadedStrings #-}
module <newmodule>.Api where

import Types 
        ( Api
        , Ticker(..)
        , Currency(..)
        , Currency'(..)
        , Markets(..)
        , Balance(..) 
        , Order(..)) 
import qualified Types as T

import           <newmodule>.Types
import           <newmodule>.Internal

defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

ping :: IO (Either Error String)
ping = return $ Left "Implement Me!"

getMarkets :: IO (Either Error Markets)

getTicker :: Markets -> IO (Either Error Tickers)
getTicker mrkt = return $ Left "Implement Me!"

getBalance :: IO (Either Error Balance)
getBalance = withKeys "keys/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!" 

buyLimit :: Order -> IO (Either Error Order)
buyLimit Order{..} = withKeys "key/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!"

sellLimit :: Order -> IO (Either Error Order)
sellLimit Order{..} = withKeys "keys/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!"

