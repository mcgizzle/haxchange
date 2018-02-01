{-# LANGUAGE OverloadedStrings #-}
module Kraken.ApiSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Types
import Kraken.Api

spec :: Spec
spec = do
        let m = MarketName (COIN BTC) (FIAT EUR)
        let badM = MarketName (COIN BTC) (COIN BTC)
        describe "GET" $ do
                it "Account balance" $ do
                        res <- getBalance
                        res `shouldSatisfy` isRight
                it "Ticker for BTC/EUR market" $ do
                        res <- getTicker m
                        res `shouldSatisfy` isRight
                it "Ticker for BTC/BTC market" $ do
                        res <- getTicker badM
                        res `shouldSatisfy` isLeft
        describe "POST" $ do
                it "Buy (correct info)" $ do
                        let o = Order m "100.00" "100.00"
                        res <- buyLimit o
                        res `shouldSatisfy` isRight
                it "Buy (incorrect info)" $ do
                        let o = Order badM "" ""
                        res <- buyLimit o
                        res `shouldSatisfy` isLeft
