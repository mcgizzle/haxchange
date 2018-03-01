{-# LANGUAGE OverloadedStrings #-}
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

import           <newmodule>.Types
import           <newmodule>.Internal

defaultOpts = Opts mempty mempty "public" mempty mempty mempty mempty

ping :: IO (Either String String)
ping = return $ Left "Implement Me!"

getTicker :: MarketName -> IO (Either String Ticker)
getTicker mrkt = return $ Left "Implement Me!"

getBalance :: IO (Either String Balance)
getBalance = withKeys "keys/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!" 

buyLimit :: Order -> IO (Either String Order)
buyLimit Order{..} = withKeys "key/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!"

sellLimit :: Order -> IO (Either String Order)
sellLimit Order{..} = withKeys "keys/<newmodule>" $ \ pubKey privKey -> return $ Left "Implement Me!"

