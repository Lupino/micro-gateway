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
import qualified Data.ByteString.Lazy as LB (ByteString)
import           Data.Int             (Int64)
import           Data.String          (IsString (..))
import qualified Data.Text            as T (unpack)
import           Network.Wai          (Request (..))
import           Network.Wreq         (Options, Response)

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
  , doRequest      :: (Options -> String -> IO (Response LB.ByteString))
                   -> Options -> String -> IO (Response LB.ByteString)
  , beforeRequest  :: Maybe String -> Request -> IO (Either String ())
  -- beforeRequest retryError req
  , afterRequest   :: Int64 -> Int -> IO ()
  -- afterRequest contentLength statusCode
  , onErrorRequest :: IO ()
  , maxRetry       :: Int
  -- set the max retry on bad gateway error
  , retryError     :: Maybe String
  }


newApp :: AppKey -> AppSecret -> Bool -> App
newApp appKey appSecret isSecure = App
  { isKeyOnPath = False
  , doRequest = error "no implement"
  , beforeRequest = \_ _ -> pure $ Right ()
  , afterRequest = \_ _ -> pure ()
  , onErrorRequest = pure ()
  , maxRetry = 3
  , retryError = Nothing
  , ..
  }


data Provider = Provider
  { getAppByKey    :: AppKey -> IO (Maybe App)
  , getAppByDomain :: Domain -> IO (Maybe App)
  , isValidDomain  :: Domain -> Bool
  , isValidKey     :: AppKey -> Bool
  }

notNull :: AppKey -> Bool
notNull (AppKey k) = not $ null k

newProvider :: Provider
newProvider = Provider
  { getAppByKey    = const $ pure Nothing
  , getAppByDomain = const $ pure Nothing
  , isValidDomain  = const False
  , isValidKey     = notNull
  }
