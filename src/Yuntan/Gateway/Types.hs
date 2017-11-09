{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Gateway.Types
  (
    App (..)
  , AppKey
  , AppSecret
  , newApp
  ) where


import qualified Data.ByteString.Lazy as LB (ByteString)
import           Data.Int             (Int64)
import           Network.Wai          (Request (..))
import           Network.Wreq         (Options, Response)

type AppKey    = String
type AppSecret = String

data App = App
  { appKey        :: AppKey
  , appSecret     :: AppSecret
  , isKeyOnPath   :: Bool
  , isSecure      :: Bool
  , doRequest     :: (Options -> String -> IO (Response LB.ByteString))
                  -> Options -> String -> IO (Response LB.ByteString)
  , beforeRequest :: Request -> IO (Either String ())
  , afterRequest  :: Int64 -> Int -> IO ()
  -- afterRequest contentLength statusCode
  }


newApp :: AppKey -> AppSecret -> Bool -> App
newApp appKey appSecret isSecure = App
  { isKeyOnPath = False
  , doRequest = error "no implement"
  , beforeRequest = \_ -> pure $ Right ()
  , afterRequest = \_ _ -> pure ()
  , ..
  }
