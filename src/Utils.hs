{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           Types

import           Control.Arrow         (first)
import           Data.Aeson            (FromJSON)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.List.Split       (splitOn)
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Network.HTTP.Client   (HttpException (..))
import           Network.Wreq.Types    (FormParam (..))
import           Util                  (unzipWith)

getNonce :: IO String
getNonce = head . splitOn "." . show <$> getPOSIXTime

timeInMilli :: IO String
timeInMilli = show . (*) 1000 . (\x -> read x :: Int) <$> getNonce

toFormParam :: Params -> [FormParam]
toFormParam params = unzipWith (:=) $ first encodeUtf8 <$> params

fromParams :: Params -> ByteString
fromParams params = encodeUtf8 $ Text.intercalate "&" ((\(x,y) -> x <> "=" <> y) <$> params)

handleExcept :: FromJSON j => HttpException -> IO (Either Error j)
handleExcept = return . Left . Exception

--------------- KEYS ----------------------------------------------
getKeys :: FilePath -> IO [ByteString]
getKeys path = B8.lines <$> B8.readFile path

withKeys :: FilePath -> (ByteString -> ByteString -> IO b) -> IO b
withKeys path f = do
        [pubKey,privKey] <- getKeys path
        f pubKey privKey
