{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module <newmodule>.Internal where

import Debug.Trace

import Utils
import Types (Opts(..))
import <newmodule>.Types

import           Network.Wreq
import           Network.HTTP.Client (HttpException)
import           Control.Lens 
import           Control.Exception as E
import           Data.Aeson.Lens 
import           Data.Aeson
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)
import           Data.List (intercalate)
import           Data.ByteString (ByteString)


-- HELPER FUNCTIONS ---------------------------------------------------------------------------------------------------
getUrl :: Opts -> String
getUrl Opts{..} = intercalate "/" [ ""
                                  , "api"
                                  , optApiVersion
                                  , optPath ]

apiSign :: Opts -> ByteString
apiSign Opts{..} = undefined

-- HEADERS -------------------------------------------------------------------------------------------------------------
getDefaults :: Opts -> Network.Wreq.Options
getDefaults opts@Opts{..} = defaults & header "Accept" .~ ["application/json"] 
                                     & params .~ optParams 

postDefaults :: Opts -> Network.Wreq.Options
postDefaults opts@Opts{..} = getDefaults opts & header "Content-Type" .~ ["application/x-www-form-urlencoded"]

-- HTTP CALLS -----------------------------------------------------------------------------------------------------------
runGetApi :: FromJSON j => Opts -> IO (Either Error j)
runGetApi opts@Opts{..} = do
        let opts' = getDefaults opts
            url = getUrl opts
        (getWith opts' url >>= asValue >>= handleRes) `E.catch` handleExcept

runPostApi :: FromJSON j => Opts -> IO (Either Error j)
runPostApi opts@Opts{..} = do
        let opts' = postDefaults opts        
            url = getUrl opts
            body = toFormParam optPost
        (postWith opts' url body >>= asValue >>= handleRes) `E.catch` handleExcept

-- HANDLERS ------------------------------------------------------------------------------------------------------------
handleRes :: FromJSON j => Response Value -> IO (Either Error j)
handleRes res = do
        let p = res ^. responseBody 
        case fromJSON p of
          Success s -> return $ Right s
          Error e -> return $ Left $ "Parse Error: " ++ e


        
