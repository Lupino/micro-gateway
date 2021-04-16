{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Data.Aeson                      (FromJSON, parseJSON,
                                                  withObject, (.!=), (.:),
                                                  (.:?))
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as LB (ByteString)
import           Data.List                       (find)
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Streaming.Network.Internal (HostPreference (Host))
import           Data.Text.Encoding              (encodeUtf8)
import qualified Data.Text.Lazy                  as LT (Text)
import qualified Data.Yaml                       as Y
import           Micro.Gateway                   (matchAny, optionsHandler,
                                                  proxyDELETEHandler,
                                                  proxyGETHandler,
                                                  proxyPOSTHandler,
                                                  proxyPUTHandler, requireApp,
                                                  verifySignature,
                                                  verifySignature',
                                                  wsProxyHandler)
import qualified Micro.Gateway                   as GW
import qualified Network.HTTP.Client             as HTTP
import           Network.URI                     (URI (..), URIAuth (..),
                                                  parseURI)
import qualified Network.Wai.Handler.Warp        as W (defaultSettings,
                                                       runSettings, setHost,
                                                       setPort)
import           Network.Wai.Handler.WebSockets  (websocketsOr)
import           Network.Wai.Middleware.Cors     (CorsResourcePolicy (..), cors,
                                                  simpleCorsResourcePolicy)
import qualified Network.WebSockets              as WS (defaultConnectionOptions)
import           Options.Applicative
import           Web.Scotty                      (ScottyM, delete, get,
                                                  middleware, options, post,
                                                  put, scottyApp)

newtype Options' = Options' {getConfigFile :: String}

parser :: Parser Options'
parser = Options' <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "config file."
                                <> value "config.yaml")

data AppConfig = AppConfig
  { key          :: GW.AppKey
  , secret       :: GW.AppSecret
  , domain       :: GW.Domain
  , baseUrl      :: String
  , secure       :: Bool
  , proxy        :: Bool -- flag of only proxy
  , wsUrl        :: Maybe String
  , replacePages :: [LT.Text]
  , replaceName  :: ByteString

  -- allow page prefix
  , allowPages   :: [LT.Text]
  -- deny page prefix
  , denyPages    :: [LT.Text]
  }

data Config = Config
  { appList     :: [AppConfig]
  , connTimeout :: Int
  -- default 10 seconds, unit seconds
  , connPool    :: Int
  -- default 10
  , host        :: String
  , port        :: Int
  }

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \o -> do
    key          <- o .:  "key"
    secret       <- o .:  "secret"
    domain       <- o .:? "domain"       .!= GW.Domain ""
    baseUrl      <- o .:  "baseUrl"
    secure       <- o .:? "secure"       .!= False
    proxy        <- o .:? "proxy"        .!= False
    wsUrl        <- o .:? "wsUrl"
    allowPages   <- o .:? "allowPages"   .!= []
    denyPages    <- o .:? "denyPages"    .!= []
    replacePages <- o .:? "replacePages" .!= []
    rname        <- o .:? "replaceName"  .!= "__KEY__"
    return AppConfig
      { replaceName = encodeUtf8 rname
      , ..
      }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    appList     <- o .:  "appList"
    connPool    <- o .:? "connPool"    .!= 10
    connTimeout <- o .:? "connTimeout" .!= 10
    host        <- o .:? "host"        .!= "127.0.0.1"
    port        <- o .:? "port"        .!= 3000
    return Config{..}


isValid :: Eq a => (AppConfig -> a) -> [AppConfig] -> a -> IO Bool
isValid f xs k = return . isJust $ find ((== k) . f) xs


main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Simple Gateway"
     <> header "simple-gateway - Simple Gateway" )

program :: Options' -> IO ()
program Options'{getConfigFile=configPath} = do
  c <- Y.decodeFileEither configPath
  case c of
    Left e     -> print e
    Right Config{..} -> do
      mgr <- HTTP.newManager HTTP.defaultManagerSettings
                { HTTP.managerConnCount = connPool
                , HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ connTimeout * 10000000
                }

      let provider = GW.newProvider
            { GW.getAppByKey = getAppAndInitail mgr appList key
            , GW.getAppByDomain = getAppAndInitail mgr appList domain
            , GW.isValidDomain = isValid domain appList
            , GW.isValidKey = isValid key appList
            }

      sapp <- scottyApp $ application provider
      let app = websocketsOr WS.defaultConnectionOptions (wsProxyHandler provider) sapp
      W.runSettings (W.setPort port . W.setHost (Host host) $ W.defaultSettings) app


getAppAndInitail :: Eq a => HTTP.Manager -> [AppConfig] -> (AppConfig -> a) -> a -> IO (Maybe GW.App)
getAppAndInitail mgr configs f k =
  case find ((==k) . f) configs of
    Nothing     -> return Nothing
    Just config -> return . Just $ toApp mgr config

toApp :: HTTP.Manager -> AppConfig -> GW.App
toApp mgr AppConfig {..} =
  (GW.newApp key secret secure proxy)
    { GW.doRequest        = processRequest mgr baseUrl
    , GW.prepareWsRequest = processWsRequest $ fromMaybe baseUrl wsUrl
    , GW.allowPages       = allowPages
    , GW.denyPages        = denyPages
    , GW.replaceKeyName   = replaceName
    , GW.replaceKeyPages  = replacePages
    }

processRequest :: HTTP.Manager -> String
               -> (HTTP.Request -> HTTP.Manager -> IO (HTTP.Response LB.ByteString))
               -> String -> IO (HTTP.Response LB.ByteString)
processRequest mgr root req uri = do
  r <- HTTP.parseRequest $ root ++ uri
  req r mgr


processWsRequest :: String -> (String -> Int -> IO ()) -> IO ()
processWsRequest baseUrl next = next (uriRegName auth) (read . drop 1 $ uriPort auth)
  where Just uri = parseURI baseUrl
        Just auth = uriAuthority uri

application :: GW.Provider -> ScottyM ()
application provider = do
  middleware $ cors (const $ Just policy)

  get     matchAny . requireApp provider $ verifySignature' proxyGETHandler
  post    matchAny . requireApp provider $ verifySignature proxyPOSTHandler
  put     matchAny . requireApp provider $ verifySignature proxyPUTHandler
  delete  matchAny . requireApp provider $ verifySignature proxyDELETEHandler
  options matchAny optionsHandler

  where policy = simpleCorsResourcePolicy
                   { corsMethods = [ "GET", "POST", "PUT", "DELETE", "OPTIONS" ]
                   , corsRequestHeaders =
                     [ "X-REQUEST-KEY"
                     , "X-REQUEST-SIGNATURE"
                     , "X-REQUEST-TIME"
                     , "X-REQUEST-TYPE"
                     , "X-REQUEST-NONCE"
                     , "Content-Type"
                     , "User-Agent"
                     , "X-Real-IP"
                     , "Host"
                     , "X-Forwarded-For"
                     , "X-URI"
                     , "X-Query-String"
                     , "X-Scheme"
                     , "Cookie"
                     , "Authorization"
                     ]
                   , corsMaxAge = Just 86400
                   }
