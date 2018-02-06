{-# LANGUAGE OverloadedStrings #-}
module <newmodule>.ApiSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Types
import <newmodule>.Api

spec :: Spec
spec = do
        describe "Connectivity" $ do
                it "Pings server" $ do
                        res <- ping
                        res `shouldSatisfy` isRight
        describe "GET" $ do
                it "Account balance" $ do
                        res <- getBalance
                        res `shouldSatisfy` isRight
                it "Ticker for BTC/EUR market" $ do
                        res <- getTicker market
                        res `shouldSatisfy` isRight
                it "Ticker for BTC/BTC market" $ do
                        res <- getTicker badMarket
                        res `shouldSatisfy` isLeft
        describe "POST" $ do
                it "Buy (correct info)" $ do
                        res <- buyLimit order
                        res `shouldSatisfy` isRight
                it "Buy (incorrect info)" $ do
                        res <- buyLimit badOrder
                        res `shouldSatisfy` isLeft
                it "Sell (correct info)" $ do
                        res <- sellLimit order
                        res `shouldSatisfy` isRight
                it "Sell (incorrect info)" $ do
                        res <- sellLimit badOrder
                        res `shouldSatisfy` isLeft
       where market    = MarketName (COIN BTC) (FIAT EUR)
             badMarket = MarketName (COIN BTC) (COIN BTC)
             order     = Order market "100.00" "100.00"
             badOrder  = Order badMarket "100.00" "100.00"
