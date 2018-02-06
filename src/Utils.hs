{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Types

import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.List.Split (splitOn)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Control.Arrow (first)
import           Network.Wreq.Types (FormParam(..))
import           Util

getNonce :: IO String
getNonce = head . splitOn "." . show <$> getPOSIXTime

timeInMilli :: IO String
timeInMilli = show . (*) 1000 . read <$> getNonce

toFormParam :: Params -> [FormParam]
toFormParam params = unzipWith (:=) $ first encodeUtf8 <$> params

fromParams :: Params -> ByteString
fromParams params = encodeUtf8 $ Text.intercalate "&" ((\(x,y) -> x <> "=" <> y) <$> params)


