{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types

import Network.Wreq
import Control.Lens 
import Data.Aeson.Lens 
import Data.Aeson

main :: IO ()
main = do
        let opts = defaults & header "Accept" .~ ["application/json"]
        res <- getWith opts "https://bittrex.com/api/v1.1/public/getmarkets" 
        let Just p = res ^? (responseBody . key "result")  
        case fromJSON p :: Result [Market] of
              Success s -> print s
              Error e -> print $ "Error: " ++ e
        return ()
