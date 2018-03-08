{-# LANGUAGE OverloadedStrings #-}
module Kraken.ApiSpec where

import           Data.Either                     (fromRight)
import           Kraken.Api
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Types

spec :: Spec
spec = do
        describe "Connectivity" $ do
                it "Pings server" $ do
                        res <- ping
                        res `shouldSatisfy` isRight
        describe "GET" $ do
                it "Gets tradeable Asset Pairs" $ do
                        res <- getMarkets
                        print $ last $ unMarkets $ fromRight (Markets []) res
                        res `shouldSatisfy` isRight
                it "Account balance" $ do
                        res <- getBalance
                        print res
                        res `shouldSatisfy` isRight
                it "Ticker for ETH/BTC market" $ do
                        res <- getTicker markets
                        print res
                        res `shouldSatisfy` isRight
        describe "POST" $ do
                it "Buy (correct info)" $ do
                        res <- buyLimit order
                        print res
                        res `shouldSatisfy` isRight
                it "Buy (incorrect info)" $ do
                        res <- buyLimit badOrder
                        res `shouldSatisfy` isLeft
                it "Sell (correct info)" $ do
                        res <- sellLimit order
                        print res
                        res `shouldSatisfy` isRight
                it "Sell (incorrect info)" $ do
                        res <- sellLimit badOrder
                        res `shouldSatisfy` isLeft
       where markets   = Markets [market]
             market    = Market (COIN ETH) (COIN BTC)
             badMarkets = Markets [badMarket]
             badMarket  = Market (COIN ETH) (COIN ETH)
             order     = Order market "100.00" "100.00"
             badOrder  = Order badMarket "100.00" "100.00"
