{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Micro.Gateway.Types
  ( App (..)
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
import qualified Data.Text.Lazy       as LT (Text, fromStrict, null, unpack)
import qualified Network.HTTP.Client  as HTTP
import           Network.Wai          (Request (..))

newtype AppKey = AppKey {unAppKey :: LT.Text}
  deriving (Eq)

instance Show AppKey where
  show = LT.unpack . unAppKey

instance IsString AppKey where
  fromString = AppKey . fromString

instance FromJSON AppKey where
  parseJSON = withText "AppKey" $ pure . AppKey . LT.fromStrict

instance ToJSON AppKey where
  toJSON (AppKey k) = toJSON k

newtype AppSecret = AppSecret {unAppSecret :: LT.Text}

instance Show AppSecret where
  show = LT.unpack . unAppSecret

instance IsString AppSecret where
  fromString = AppSecret . fromString

instance FromJSON AppSecret where
  parseJSON = withText "AppSecret" $ pure . AppSecret . LT.fromStrict

instance ToJSON AppSecret where
  toJSON (AppSecret s) = toJSON s

newtype Domain = Domain {unDomain :: LT.Text}
  deriving (Eq)

instance Show Domain where
  show = LT.unpack . unDomain

instance IsString Domain where
  fromString = Domain . fromString

instance FromJSON Domain where
  parseJSON = withText "Domain" $ pure . Domain . LT.fromStrict

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

  -- allow page prefix only effect get pages
  , allowPages :: [LT.Text]
  -- deny page prefix only effect allow pages
  , denyPages :: [LT.Text]
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
  , denyPages = []
  , ..
  }


data Provider = Provider
  { getAppByKey    :: AppKey -> IO (Maybe App)
  , getAppByDomain :: Domain -> IO (Maybe App)
  , isValidDomain  :: Domain -> IO Bool
  , isValidKey     :: AppKey -> IO Bool
  }

notNull :: AppKey -> Bool
notNull (AppKey k) = not $ LT.null k

newProvider :: Provider
newProvider = Provider
  { getAppByKey    = const $ pure Nothing
  , getAppByDomain = const $ pure Nothing
  , isValidDomain  = return . const False
  , isValidKey     = return . notNull
  }
