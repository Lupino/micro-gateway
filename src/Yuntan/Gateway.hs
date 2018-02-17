{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Gateway
  (
    module Yuntan.Gateway.Types
  , requireApp
  , verifySignature
  , verifySignature'
  , matchAny
  , proxyPOSTHandler
  , proxyPUTHandler
  , proxyGETHandler
  , proxyDELETEHandler
  , optionsHandler
  ) where

import           Control.Exception      (try)
import           Control.Lens           ((&), (.~), (^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (..), decode, toJSON)
import qualified Data.ByteString.Char8  as B (ByteString, append, concat, pack,
                                              unpack)
import qualified Data.ByteString.Lazy   as LB (ByteString, empty, fromStrict,
                                               length, toStrict)
import           Data.CaseInsensitive   (CI, original)
import           Data.HashMap.Strict    (delete, insert, lookupDefault)
import           Data.Hex               (hex)
import           Data.Int               (Int64)
import           Data.Maybe             (fromMaybe)
import           Data.Text              as T (Text, pack, toUpper, unpack)
import qualified Data.Text.Lazy         as LT (Text, null, pack, toStrict,
                                               unpack)
import           Network.HTTP.Client    (HttpException (..),
                                         HttpExceptionContent (..))
import           Network.HTTP.Types     (ResponseHeaders, status204, status500,
                                         status504, statusCode)
import           Network.Wai            (Request (rawPathInfo, rawQueryString, requestMethod))
import qualified Network.Wreq           as Wreq
import           System.Log.Logger      (errorM)
import           Text.Read              (readMaybe)
import           Web.Scotty             (ActionM, Param, RoutePattern, body,
                                         function, header, param, params, raw,
                                         request, rescue, setHeader, status)
import           Yuntan.Gateway.Types
import           Yuntan.Gateway.Utils
import           Yuntan.Utils.Scotty    (err, errBadRequest, errNotFound)
import           Yuntan.Utils.Signature (hmacSHA256, signJSON, signParams,
                                         signRaw)


proxyPOSTHandler :: App -> ActionM ()
proxyPOSTHandler app = do
  wb <- body
  responseWreq app $ flip' Wreq.postWith wb

proxyPUTHandler :: App -> ActionM ()
proxyPUTHandler app = do
  wb <- body
  responseWreq app $ flip' Wreq.putWith wb

proxyGETHandler :: App -> ActionM ()
proxyGETHandler app = responseWreq app Wreq.getWith

proxyDELETEHandler :: App -> ActionM ()
proxyDELETEHandler app = do
  wb <- body
  responseWreq app $ flip' (Wreq.customPayloadMethodWith "DELETE") wb

mergeResponseHeaders :: [CI B.ByteString] -> ResponseHeaders -> ActionM ()
mergeResponseHeaders _ [] = return ()
mergeResponseHeaders k ((n, v):xs) =
  if n `elem` k then do
                setHeader (b2t $ original n) $ b2t v
                mergeResponseHeaders k xs
                else mergeResponseHeaders k xs

responseWreq :: App -> (Wreq.Options -> String -> IO (Wreq.Response LB.ByteString)) -> ActionM ()
responseWreq app req = do
  ret <- liftIO . beforeRequest app =<< request
  case ret of
    Left e  -> err status500 e
    Right _ -> responseWreq' app req

responseWreq' :: App -> (Wreq.Options -> String -> IO (Wreq.Response LB.ByteString)) -> ActionM ()
responseWreq' app@App{isKeyOnPath=isOnPath} req = do
  uri <- dropKeyFromPath isOnPath <$> param "rawuri"
  opts <- getWreqOptions
  e <- liftIO . try $ doRequest app req opts uri
  case e of
    Left (HttpExceptionRequest _ content) ->
      case content of
        (StatusCodeException r dat) -> do
          let hdrs = r ^. Wreq.responseHeaders
              st   = r ^. Wreq.responseStatus

          output hdrs st $ LB.fromStrict dat
        ResponseTimeout -> do
          status status504
          raw LB.empty
        other -> do
          liftIO $ errorM "Yuntan.Gateway.Handler" (show other)
          status status500
          raw LB.empty

    Left (InvalidUrlException _ _) -> do
      status status500
      raw LB.empty
    Right r  -> do
      let hdrs = r ^. Wreq.responseHeaders
          st   = r ^. Wreq.responseStatus
          dat  = r ^. Wreq.responseBody

      output hdrs st dat

  where output hdrs st dat = do
          let len  = LB.length dat

          status st
          setHeader "Content-Length" . LT.pack . show $ len
          mergeResponseHeaders ["Content-Type"] hdrs
          raw dat
          liftIO . afterRequest app len $ statusCode st

mergeRequestHeaders :: [CI B.ByteString] -> ActionM Wreq.Options
mergeRequestHeaders [] = return Wreq.defaults
mergeRequestHeaders (x:xs) = do
  hdr <- header (b2t $ original x)
  hdrs <- mergeRequestHeaders xs
  case hdr of
    Just hd -> return $ hdrs & Wreq.header x .~ [t2b hd]
    Nothing -> return hdrs

getWreqOptions :: ActionM Wreq.Options
getWreqOptions =
  mergeRequestHeaders [ "Content-Type"
                      , "User-Agent"
                      , "X-REQUEST-KEY"
                      , "X-Real-IP"
                      , "Host"
                      , "X-Forwarded-For"
                      , "X-URI"
                      , "X-Query-String"
                      , "X-Scheme"
                      ]

verifySignature' :: (App -> ActionM()) -> App -> ActionM ()
verifySignature' proxy app@App{isSecure=secure} =
  if secure then verifySignature proxy app
            else proxy app

verifySignature :: (App -> ActionM ()) -> App -> ActionM ()
verifySignature proxy app@App{appSecret=sec, appKey=key, isKeyOnPath=isOnPath}= do
  ct <- header "Content-Type"
  sec' <- signSecretKey . B.pack $ show sec
  case sec' of
    Left e -> errBadRequest e
    Right secret ->
      case ct of
        Just "application/json"                  -> doVerifyJSON secret
                                                  $ doVerifyParams secret
                                                  $ doVerifyRaw secret errorInvalidSignature

        Just "application/x-www-form-urlencoded" -> doVerifyParams secret errorInvalidSignature
        _                                        -> doVerifyParams secret $ doVerifyRaw secret errorInvalidSignature

  where doVerifyJSON :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyJSON secret next = do
          hsign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          hts   <- LT.toStrict <$> headerOrParam "X-REQUEST-TIME" "timestamp"
          wb    <- body
          sp <- dropKeyFromPath isOnPath <$> param "pathname"
          case (decode wb :: Maybe Value) of
            Just (Object v) -> do
              let (String sign) = lookupDefault (String hsign) "sign" v
                  (String ts) = lookupDefault (String hts) "timestamp" v
                  v' = delete "sign" $ insert "timestamp" (String ts)
                                     $ insert "key" (toJSON key)
                                     $ insert "pathname" (String sp) v
                  exceptSign = signJSON secret (Object v')

              verifyTime (T.unpack ts) $ equalSign exceptSign sign next

            _ -> next

        equalSign :: B.ByteString -> T.Text -> ActionM () -> ActionM ()
        equalSign except sign next =
          if B.unpack except == T.unpack (T.toUpper sign) then proxy app
                                                          else next

        doVerifyRaw :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyRaw secret next = do
          sign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          timestamp <- headerOrParam "X-REQUEST-TIME" "timestamp"
          sp <- dropKeyFromPath isOnPath <$> param "pathname"
          wb <- body
          let exceptSign = signRaw secret [ ("key", B.pack $ show key)
                                          , ("timestamp", t2b timestamp)
                                          , ("raw", LB.toStrict wb)
                                          , ("pathname", sp)
                                          ]

          verifyTime (LT.unpack timestamp) $ equalSign exceptSign sign next

        doVerifyParams :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyParams secret next = do
          sign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          timestamp <- headerOrParam "X-REQUEST-TIME" "timestamp"
          vv <- params
          sp <- dropKeyFromPath isOnPath <$> param "pathname"
          let exceptSign = signParams secret $ set "key" (LT.pack $ show key)
                                             $ set "timestamp" timestamp
                                             $ set "pathname" sp
                                             $ remove "sign"
                                             $ remove "rawuri" vv

          verifyTime (LT.unpack timestamp) $ equalSign exceptSign sign next

          where remove :: LT.Text -> [Param] -> [Param]
                remove _ []          = []
                remove k' ((k, v):xs) = if k' == k then xs
                                                   else (k, v) : remove k' xs

                has :: LT.Text -> [Param] -> Bool
                has _ []           = False
                has k' ((k, _):xs) = (k' == k) || has k' xs

                set :: LT.Text -> LT.Text -> [Param] -> [Param]
                set k v vv = if has k vv then vv
                                         else (k, v):vv

        signSecretKey :: B.ByteString -> ActionM (Either String B.ByteString)
        signSecretKey secret = do
          tp <- headerOrParam "X-REQUEST-TYPE" "type"
          case tp of
            "JSAPI" -> do
              nonce <- headerOrParam "X-REQUEST-NONCE" "nonce"
              ts <- headerOrParam "X-REQUEST-TIME" "timestamp"
              sp <- dropKeyFromPath isOnPath <$> param "pathname"
              method <- requestMethod <$> request
              if LT.null nonce then return (Left "Invalid REQUEST NONCE")
                               else return . Right . hex . hmacSHA256 (t2b nonce) $ B.concat [secret, method, sp, t2b ts]

            _ -> return (Right secret)

        errorInvalidSignature :: ActionM ()
        errorInvalidSignature = errBadRequest "Invalid REQUEST SIGNATURE"

        errorTimeout :: ActionM ()
        errorTimeout = errBadRequest "SIGNATURE TIMEOUT"

        verifyTime :: String -> ActionM () -> ActionM ()
        verifyTime ts' next = do
          let ts = fromMaybe (0::Int64) $ readMaybe ts'
          t <- liftIO getEpochTime
          if t - 300 < ts then next
                          else errorTimeout

optionsHandler :: ActionM ()
optionsHandler = status status204 >> raw LB.empty

headerOrParam :: LT.Text -> LT.Text -> ActionM LT.Text
headerOrParam hk pk = do
  hv <- header hk
  case hv of
    Just hv' -> return hv'
    Nothing  -> param pk `rescue` const (return "")

requireApp :: Provider -> (App -> ActionM ()) -> ActionM ()
requireApp Provider{..} proxy = do
  key <- AppKey . LT.unpack <$> headerOrParam "X-REQUEST-KEY" "key"
  if isValidKey key then doGetAppFromKey key
                    else doGetAppFromPath
  where doGetAppFromKey :: AppKey -> ActionM ()
        doGetAppFromKey key = process key =<< liftIO (getAppByKey key)

        doGetAppFromPath :: ActionM ()
        doGetAppFromPath = do
          key <- AppKey . takeKeyFromPath <$> param "pathname"
          if isValidKey key then do
            app <- liftIO $ getAppByKey key
            case app of
              Nothing   -> errorNotFound key
              Just app' -> proxy app' {isKeyOnPath=True}
          else doGetAppByDomain

        doGetAppByDomain :: ActionM ()
        doGetAppByDomain = do
          host <- Domain . LT.unpack . fromMaybe "" <$> header "Host"
          if isValidDomain host then process host =<< liftIO (getAppByDomain host)
                                else errorRequired

        process :: Show a => a -> Maybe App -> ActionM ()
        process n Nothing    = errorNotFound n
        process _ (Just app) = proxy app

        errorRequired :: ActionM ()
        errorRequired = errBadRequest "KEY is required."

        errorNotFound :: Show a => a -> ActionM ()
        errorNotFound d = errNotFound $ "APP " ++ show d ++ " is not found."

matchAny :: RoutePattern
matchAny = function $ \req ->
  Just [ ("rawuri",  b2t $ rawPathInfo req `B.append` rawQueryString req)
       , ("pathname", b2t $ rawPathInfo req)
       ]
