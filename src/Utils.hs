module Utils where

import Data.List.Split (splitOn)
import Data.Time.Clock.POSIX (getPOSIXTime)

getNonce :: IO String
getNonce = head . splitOn "." . show <$> getPOSIXTime

