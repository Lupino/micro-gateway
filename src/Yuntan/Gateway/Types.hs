{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Gateway.Types
  (
    App (..)
  , AppKey (..)
  , AppSecret (..)
  , Domain (..)
  , newApp

  , Provider (..)
  , newProvider
  ) where

import           Data.Aeson           (FromJSON (..), ToJSON (..), withText)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LB (ByteString)
import           Data.Int             (Int64)
import           Data.String          (IsString (..))
import qualified Data.Text            as T (unpack)
import qualified Data.Text.Lazy       as LT (Text)
import qualified Network.HTTP.Client  as HTTP
import           Network.Wai          (Request (..))

newtype AppKey    = AppKey String
  deriving (Eq)

instance Show AppKey where
  show (AppKey k) = k

instance IsString AppKey where
  fromString = AppKey . fromString

instance FromJSON AppKey where
  parseJSON = withText "AppKey" $ \t -> pure (AppKey $ T.unpack t)

instance ToJSON AppKey where
  toJSON (AppKey k) = toJSON k

newtype AppSecret = AppSecret String

instance Show AppSecret where
  show (AppSecret s) = s

instance IsString AppSecret where
  fromString = AppSecret . fromString

instance FromJSON AppSecret where
  parseJSON = withText "AppSecret" $ \t -> pure (AppSecret $ T.unpack t)

instance ToJSON AppSecret where
  toJSON (AppSecret s) = toJSON s

newtype Domain    = Domain String

instance Show Domain where
  show (Domain d) = d

instance IsString Domain where
  fromString = Domain . fromString

instance FromJSON Domain where
  parseJSON = withText "Domain" $ \t -> pure (Domain $ T.unpack t)

instance ToJSON Domain where
  toJSON (Domain d) = toJSON d

data App = App
  { appKey         :: AppKey
  , appSecret      :: AppSecret
  , isKeyOnPath    :: Bool
  , isSecure       :: Bool
  , onlyProxy      :: Bool
  , doRequest      :: (HTTP.Request -> HTTP.Manager -> IO (HTTP.Response LB.ByteString))
                   -> String -> IO (HTTP.Response LB.ByteString)
  , beforeRequest  :: Maybe String -> Request -> IO (Either String ())
  -- beforeRequest retryError req
  , afterRequest   :: Int64 -> Int -> IO ()
  -- afterRequest contentLength statusCode
  , onErrorRequest :: IO ()
  , maxRetry       :: Int
  -- set the max retry on bad gateway error
  , retryError     :: Maybe String
  , prepareWsRequest :: (String -> Int -> IO ()) -> IO ()
  --
  , replaceKeyPages :: [LT.Text]
  , replaceKeyName :: ByteString

  -- allow page prefix
  , allowPages :: [LT.Text]
  }


newApp :: AppKey -> AppSecret -> Bool -> Bool -> App
newApp appKey appSecret isSecure onlyProxy = App
  { isKeyOnPath = False
  , doRequest = error "no implement"
  , beforeRequest = \_ _ -> pure $ Right ()
  , afterRequest = \_ _ -> pure ()
  , onErrorRequest = pure ()
  , maxRetry = 3
  , retryError = Nothing
  , prepareWsRequest = error "no implement"
  , replaceKeyPages = []
  , replaceKeyName = "__KEY__"
  , allowPages = []
  , ..
  }


data Provider = Provider
  { getAppByKey    :: AppKey -> IO (Maybe App)
  , getAppByDomain :: Domain -> IO (Maybe App)
  , isValidDomain  :: Domain -> IO Bool
  , isValidKey     :: AppKey -> IO Bool
  }

notNull :: AppKey -> Bool
notNull (AppKey k) = not $ null k

newProvider :: Provider
newProvider = Provider
  { getAppByKey    = const $ pure Nothing
  , getAppByDomain = const $ pure Nothing
  , isValidDomain  = return . const False
  , isValidKey     = return . notNull
  }
