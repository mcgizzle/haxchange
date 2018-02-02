module Utils where

import Types

import Data.List.Split (splitOn)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Arrow (first)
import Data.Text.Encoding (encodeUtf8)
import Network.Wreq.Types (FormParam(..))
import Util

getNonce :: IO String
getNonce = head . splitOn "." . show <$> getPOSIXTime

toFormParam :: Params -> [FormParam]
toFormParam params = unzipWith (:=) $ first encodeUtf8 <$> params
