{-# LANGUAGE OverloadedStrings #-}
module Kraken.ApiSpec where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Types
import Kraken.Api

spec :: Spec
spec = do
        let m = MarketName (COIN BTC) (FIAT EUR)
        describe "GET" $ do
                it "Gets account balance from Kraken" $ do
                        res <- getBalance
                        res `shouldSatisfy` isRight
                it "Gets ticker for BTC/EUR Market" $ do
                        res <- getTicker m
                        res `shouldSatisfy` isRight
        describe "POST" $ do
                it "Places (test) buy" $ do
                        let o = Order m "100.00" "100.00"
                        res <- buyLimit o
                        res `shouldSatisfy` isRight
