{-# LANGUAGE OverloadedStrings #-}
module Haxchange.Utils where

import           Haxchange.Types

import           Control.Arrow         (first)
import qualified Data.Aeson.Types      as A
import qualified Data.Attoparsec.Text  as AP
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.List.Split       (splitOn)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Network.HTTP.Client   (HttpException (..))
import           Network.Wreq.Types    (FormParam (..))
import           Util                  (unzipWith)

getNonce :: IO String
getNonce = head . splitOn "." . show <$> getPOSIXTime

--FIXME: This is a weird hack to get the milliseconds correct for Binance

timeInMilli :: IO String
timeInMilli = show . flip (-) 2500 . (* 1000) . (\x -> floor x :: Int) <$> getPOSIXTime

toFormParam :: Params -> [FormParam]
toFormParam params = unzipWith (:=) $ first encodeUtf8 <$> params

fromParams :: Params -> ByteString
fromParams = encodeUtf8 . Text.intercalate "&" . unzipWith (\x y -> x <> "=" <> y)

handleExcept :: HttpException -> IO (Either Error j)
handleExcept = return . Left . Exception

attoAeson :: AP.Parser a -> Text -> A.Parser a
attoAeson parser x = case AP.parseOnly parser x of
                        Left err -> fail err
                        Right r  -> pure r

monoidParse :: (Monoid a, Applicative f) => AP.Parser a -> Text -> f a
monoidParse parser x = case AP.parseOnly parser x of
              Left _  -> pure mempty
              Right r -> pure r

--------------- KEYS ----------------------------------------------
getKeys' :: FilePath -> IO [ByteString]
getKeys' path = B8.lines <$> B8.readFile path

getKeys :: FilePath -> IO APIKeys
getKeys path = do
        [pubKey,privKey] <- getKeys' path
        return $ APIKeys pubKey privKey
