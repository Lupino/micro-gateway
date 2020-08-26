{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yuntan.Gateway
  ( module Yuntan.Gateway.Types
  , requireApp
  , verifySignature
  , verifySignature'
  , matchAny
  , proxyPOSTHandler
  , proxyPUTHandler
  , proxyGETHandler
  , proxyDELETEHandler
  , optionsHandler
  , wsProxyHandler
  ) where


import           Control.Concurrent            (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM.TChan  (newTChanIO, readTChan,
                                                writeTChan)
import           Control.Concurrent.STM.TVar   (newTVarIO, readTVar, readTVarIO,
                                                writeTVar)
import           Control.Exception             (SomeException, try)
import           Control.Monad                 (forever, void, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.STM             (atomically)
import           Crypto.Signature              (hmacSHA256, signJSON,
                                                signParams, signRaw)
import           Data.Aeson                    (Value (..), decode, object,
                                                toJSON, (.=))
import           Data.Bifunctor                (first)
import           Data.Binary.Builder           (toLazyByteString)
import qualified Data.ByteString.Char8         as B (ByteString, append,
                                                     breakSubstring, concat,
                                                     drop, dropWhile, length,
                                                     null, pack, takeWhile,
                                                     unpack)
import qualified Data.ByteString.Lazy          as LB (ByteString, empty,
                                                      fromStrict, length,
                                                      toStrict)
import           Data.CaseInsensitive          (CI, mk, original)
import           Data.HashMap.Strict           (delete, insert, lookupDefault)
import           Data.Int                      (Int64)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     as T (Text, unpack)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                as LT (Text, fromStrict, length,
                                                      null, pack, take,
                                                      toStrict, unpack)
import           Network.HTTP.Client           (Cookie (..), CookieJar,
                                                HttpException (..),
                                                HttpExceptionContent (..),
                                                destroyCookieJar)
import qualified Network.HTTP.Client           as HTTP
import           Network.HTTP.Types            (Method, RequestHeaders,
                                                ResponseHeaders, Status,
                                                status204, status400, status404,
                                                status500, status502, status503,
                                                status504, statusCode,
                                                urlDecode)
import           Network.Wai                   (Request (rawPathInfo, rawQueryString, requestMethod))
import qualified Network.WebSockets            as WS (Headers, RequestHead (..),
                                                      ServerApp, acceptRequest,
                                                      defaultConnectionOptions,
                                                      pendingRequest,
                                                      receiveDataMessage,
                                                      rejectRequest,
                                                      runClientWith,
                                                      sendDataMessage)
import           Network.WebSockets.Connection as WS (pingThread)
import           System.Log.Logger             (errorM)
import           Text.Read                     (readMaybe)
import           Web.Cookie                    (SetCookie (..),
                                                defaultSetCookie, parseCookies,
                                                renderSetCookie)
import           Web.Scotty                    (ActionM, Param, RoutePattern,
                                                addHeader, body, function,
                                                header, json, param, params,
                                                raw, request, rescue, setHeader,
                                                status)
import           Yuntan.Gateway.Types
import           Yuntan.Gateway.Utils



err :: Status -> String -> ActionM ()
err st msg = status st >> json (object ["err" .= msg])

errBadRequest :: String -> ActionM ()
errBadRequest = err status400

errNotFound :: String -> ActionM ()
errNotFound = err status404

proxyPOSTHandler :: App -> ActionM ()
proxyPOSTHandler app = do
  wb <- body
  responseHTTP app $ prepareHTTPRequest "POST" (Just wb)

proxyPUTHandler :: App -> ActionM ()
proxyPUTHandler app = do
  wb <- body
  responseHTTP app $ prepareHTTPRequest "PUT" (Just wb)

proxyGETHandler :: App -> ActionM ()
proxyGETHandler app = responseHTTP app (prepareHTTPRequest "GET" Nothing)

proxyDELETEHandler :: App -> ActionM ()
proxyDELETEHandler app = do
  wb <- body
  responseHTTP app $ prepareHTTPRequest "DELETE" (Just wb)

prepareHTTPRequest
  :: Method -> Maybe LB.ByteString
  -> HTTP.Request -> HTTP.Manager -> IO (HTTP.Response LB.ByteString)
prepareHTTPRequest m Nothing req =
  HTTP.httpLbs (req {HTTP.method=m})
prepareHTTPRequest m (Just bs) req =
  HTTP.httpLbs (req {HTTP.method=m, HTTP.requestBody = HTTP.RequestBodyLBS bs })

mergeResponseHeaders :: [CI B.ByteString] -> ResponseHeaders -> ActionM ()
mergeResponseHeaders _ [] = return ()
mergeResponseHeaders k ((n, v):xs) =
  if n `elem` k then do
                setHeader (b2t $ original n) $ b2t v
                mergeResponseHeaders k xs
                else mergeResponseHeaders k xs

cookie2SetCookie :: Cookie -> SetCookie
cookie2SetCookie Cookie {..}= defaultSetCookie
  { setCookieName = cookie_name
  , setCookieValue = cookie_value
  , setCookiePath = Just cookie_path
  , setCookieExpires = Just cookie_expiry_time
  -- , setCookieMaxAge =
  -- , setCookieDomain = Just cookie_domain
  , setCookieHttpOnly = cookie_http_only
  , setCookieSecure = cookie_secure_only
  -- , setCookieSameSite =
  }

mergeSetCookie :: CookieJar -> ActionM ()
mergeSetCookie cj = do
  mapM_ (addHeader "Set-Cookie") cookies
  where cookies = map (LT.fromStrict . decodeUtf8 . LB.toStrict . toLazyByteString . renderSetCookie . cookie2SetCookie) $ destroyCookieJar cj

getPathName :: App -> ActionM LT.Text
getPathName App{isKeyOnPath=isOnPath} = do
  dropKeyFromPath isOnPath <$> param "pathname"

getRawUri :: App -> ActionM LT.Text
getRawUri App{isKeyOnPath=isOnPath} =
  dropKeyFromPath isOnPath <$> param "rawuri"

responseHTTP :: App -> (HTTP.Request -> HTTP.Manager -> IO (HTTP.Response LB.ByteString)) -> ActionM ()
responseHTTP app req = do
  ret <- liftIO . beforeRequest app (retryError app) =<< request
  case ret of
    Left e  -> err status500 e
    Right _ -> responseHTTP' app req

responseHTTP' :: App -> (HTTP.Request -> HTTP.Manager -> IO (HTTP.Response LB.ByteString)) -> ActionM ()
responseHTTP' app@App{onErrorRequest=onError} req = do
  uri <- LT.unpack <$> getRawUri app

  rheaders <- mergeRequestHeaders
    [ "Content-Type"
    , "User-Agent"
    , "X-REQUEST-KEY"
    , "X-Real-IP"
    , "Host"
    , "X-Forwarded-For"
    , "X-URI"
    , "X-Query-String"
    , "X-Scheme"
    , "Cookie"
    , "Authorization"
    ]

  e <- liftIO . try $ doRequest app (prepareReq rheaders req) uri
  case e of
    Left (HttpExceptionRequest _ content) ->
      case content of
        (StatusCodeException r dat) -> do
          let hdrs = HTTP.responseHeaders r
              st   = HTTP.responseStatus r
              cookie = HTTP.responseCookieJar r

          output hdrs st cookie $ LB.fromStrict dat
          when (st == status502 || st == status504 || st == status503)
            $ liftIO onError
        ResponseTimeout -> do
          status status504
          raw LB.empty
          liftIO onError
        other -> do
          liftIO $ errorM "Yuntan.Gateway.Handler" (show other)
          liftIO onError
          if maxRetry app <= 1 then do
            status status502
            raw LB.empty
          else do
            responseHTTP (app
              { maxRetry = maxRetry app - 1
              , retryError = Just (show other)
              }) req

    Left (InvalidUrlException _ _) -> do
      status status500
      raw LB.empty
    Right r  -> do
      let hdrs = HTTP.responseHeaders r
          st   = HTTP.responseStatus r
          dat  = HTTP.responseBody r
          cookie = HTTP.responseCookieJar r

      output hdrs st cookie dat

  where output hdrs st cookie dat' = do
          pathname <- getPathName app

          let dat = replaceData pathname dat'
              len = LB.length dat

          status st
          setHeader "Content-Length" . LT.pack . show $ len
          mergeResponseHeaders ["Content-Type", "Location", "Date"] hdrs
          mergeSetCookie cookie

          raw dat

          liftIO . afterRequest app len $ statusCode st

        prepareReq h f req' mgr = f (req' {HTTP.requestHeaders = h, HTTP.redirectCount = 0}) mgr

        rkName = replaceKeyName app
        key = B.pack . show $ appKey app

        replaceData pathname dat =
          if pathname `elem` replaceKeyPages app
            then LB.fromStrict $ replaceByteString rkName key $ LB.toStrict dat
            else dat

replaceByteString :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceByteString sep sub = go . B.breakSubstring sep
  where len = B.length sep
        go :: (B.ByteString, B.ByteString) -> B.ByteString
        go (bs, "") = bs
        go (bs, ts) = bs <> sub <> go (B.breakSubstring sep $ B.drop len ts)


mergeRequestHeaders :: [CI B.ByteString] -> ActionM RequestHeaders
mergeRequestHeaders [] = return []
mergeRequestHeaders (x:xs) = do
  hdr <- header (b2t $ original x)
  hdrs <- mergeRequestHeaders xs
  case hdr of
    Just hd -> return ((x, encodeUtf8 $ LT.toStrict hd):hdrs)
    Nothing -> return hdrs


verifySignature' :: (App -> ActionM()) -> App -> ActionM ()
verifySignature' proxy app@App{isSecure=False} = proxy app
verifySignature' proxy app@App{isSecure=True}  = do
  sp <- getPathName app
  if isAllowPages (allowPages app) sp
    then proxy app else verifySignature proxy app

  where isAllowPages :: [LT.Text] -> LT.Text -> Bool
        isAllowPages [] _ = False
        isAllowPages (x:xs) p
          | x == p = True
          | x == LT.take (LT.length x) p = True
          | otherwise = isAllowPages xs p

verifySignature :: (App -> ActionM ()) -> App -> ActionM ()
verifySignature proxy app@App{onlyProxy = True} = proxy app
verifySignature proxy app@App{appSecret=sec, appKey=key}= do
  ct <- header "Content-Type"
  sec' <- signSecretKey . B.pack $ show sec
  case sec' of
    Left e -> errBadRequest e
    Right secret ->
      case ct of
        Just "application/json"                  -> doVerifyJSON secret
                                                  $ doVerifyRaw secret errorInvalidSignature

        Just "application/x-www-form-urlencoded" -> doVerifyParams secret errorInvalidSignature
        Just "application/octet-stream"          -> doVerifyRaw secret errorInvalidSignature
        _                                        -> doVerifyParams secret
                                                  $ doVerifyRaw secret errorInvalidSignature

  where doVerifyJSON :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyJSON secret next = do
          hsign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          hts   <- LT.toStrict <$> headerOrParam "X-REQUEST-TIME" "timestamp"
          wb    <- body
          sp    <- getPathName app
          case (decode wb :: Maybe Value) of
            Just (Object v) -> do
              let (String sign) = lookupDefault (String hsign) "sign" v
                  (String ts) = lookupDefault (String hts) "timestamp" v
                  v' = delete "sign" $ insert "timestamp" (String ts)
                                     $ insert "key" (toJSON key)
                                     $ insert "pathname" (String $ LT.toStrict sp) v
                  exceptSign = signJSON secret (Object v')

              verifyTime (T.unpack ts) $ equalSign exceptSign sign next

            _ -> next

        equalSign :: CI B.ByteString -> T.Text -> ActionM () -> ActionM ()
        equalSign except sign next =
          if except == mk (encodeUtf8 sign) then proxy app
                                            else next

        doVerifyRaw :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyRaw secret next = do
          sign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          timestamp <- headerOrParam "X-REQUEST-TIME" "timestamp"
          sp <- getPathName app
          wb <- body
          let exceptSign = signRaw secret [ ("key", B.pack $ show key)
                                          , ("timestamp", t2b timestamp)
                                          , ("raw", LB.toStrict wb)
                                          , ("pathname", t2b sp)
                                          ]

          verifyTime (LT.unpack timestamp) $ equalSign exceptSign sign next

        doVerifyParams :: B.ByteString -> ActionM () -> ActionM ()
        doVerifyParams secret next = do
          sign <- LT.toStrict <$> headerOrParam "X-REQUEST-SIGNATURE" "sign"
          timestamp <- headerOrParam "X-REQUEST-TIME" "timestamp"
          vv <- params
          sp <- getPathName app
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
                set k v vv = if has k vv then set k v $ remove k vv
                                         else (k, v):vv

        signSecretKey :: B.ByteString -> ActionM (Either String B.ByteString)
        signSecretKey secret = do
          tp <- headerOrParam "X-REQUEST-TYPE" "type"
          case tp of
            "JSAPI" -> do
              nonce <- headerOrParam "X-REQUEST-NONCE" "nonce"
              ts <- headerOrParam "X-REQUEST-TIME" "timestamp"
              sp <- getPathName app
              method <- requestMethod <$> request
              if LT.null nonce then return (Left "Invalid REQUEST NONCE")
                               else return . Right . original . hmacSHA256 (t2b nonce)
                                      $ B.concat [secret, method, t2b sp, t2b ts]

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

cookieKey :: ActionM LT.Text
cookieKey = do
  hv <- header "Cookie"
  case hv of
    Just hv' -> do
      let cookies = map (first mk) $ parseCookies $ encodeUtf8 $ LT.toStrict hv'
          ckey = LT.fromStrict . decodeUtf8 . fromMaybe "" $ getFromHeader cookies "key"
      return ckey
    Nothing -> return ""

headerOrParam :: LT.Text -> LT.Text -> ActionM LT.Text
headerOrParam hk pk = do
  hv <- header hk
  case hv of
    Just hv' -> return hv'
    Nothing  -> param pk `rescue` const (return "")

requireApp :: Provider -> (App -> ActionM ()) -> ActionM ()
requireApp Provider{..} proxy = doGetAppByDomain
  where doGetAppFromPath :: ActionM ()
        doGetAppFromPath = do
          key <- AppKey . takeKeyFromPath <$> param "pathname"
          valid <- liftIO $ isValidKey key
          if valid then do
            app <- liftIO $ getAppByKey key
            case app of
              Nothing   -> errorRequired
              Just app' -> proxy app' {isKeyOnPath=True}
          else errorRequired

        doGetAppByDomain :: ActionM ()
        doGetAppByDomain = do
          host <- Domain . LT.unpack . fromMaybe "" <$> header "Host"
          valid <- liftIO $ isValidDomain host
          if valid then process host =<< liftIO (getAppByDomain host)
                   else doGetAppByHeaderOrParam

        doGetAppByHeaderOrParam :: ActionM ()
        doGetAppByHeaderOrParam = do
          hkey <- headerOrParam "X-REQUEST-KEY" "key"
          ckey <- cookieKey

          let key = AppKey . LT.unpack $ if LT.null hkey then ckey else hkey

          valid <- liftIO $ isValidKey key
          if valid then process key =<< liftIO (getAppByKey key)
                   else doGetAppFromPath

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
       , ("pathname", b2t $ urlDecode True $ rawPathInfo req)
       ]

--------------------------------------------------------------------------------
getFromHeader :: WS.Headers -> CI B.ByteString -> Maybe B.ByteString
getFromHeader [] _ = Nothing
getFromHeader ((x, y):xs) k | x == k = Just y
                            | otherwise = getFromHeader xs k

removeFromHeader :: CI B.ByteString -> WS.Headers -> WS.Headers
removeFromHeader _ []         = []
removeFromHeader k (h@(x,_):xs)
  | x == k = xs
  | otherwise = h : removeFromHeader k xs

getParam :: B.ByteString -> B.ByteString -> Maybe B.ByteString
getParam k = go . snd . B.breakSubstring k
  where go :: B.ByteString -> Maybe B.ByteString
        go "" = Nothing
        go v  = go1 . B.drop 1 $ B.takeWhile (/='&') $ B.dropWhile (/='=') v

        go1 :: B.ByteString -> Maybe B.ByteString
        go1 "" = Nothing
        go1 v  = Just v

getFromHeaderOrParam :: WS.Headers -> B.ByteString -> CI B.ByteString -> B.ByteString -> B.ByteString
getFromHeaderOrParam headers rawuri hk k =
  fromMaybe (fromMaybe "" $ getParam k rawuri) $ getFromHeader headers hk

wsProxyHandler :: Provider -> WS.ServerApp
wsProxyHandler Provider{..} pendingConn =
  withDomainOr
    $ withKeyOr key
    $ withKeyOr ckey
    $ withKeyOr pkey
    $ rejectRequest "KEY is required"
  where requestHead = WS.pendingRequest pendingConn
        rawuri = WS.requestPath requestHead
        pathname = b2t $ urlDecode True $ B.takeWhile (/='?') rawuri
        headers = WS.requestHeaders requestHead
        host = Domain . B.unpack . fromMaybe "" $ getFromHeader headers "Host"

        key = AppKey
          . B.unpack
          $ getFromHeaderOrParam headers rawuri "X-REQUEST-KEY" "key"

        cookies = map (first mk) $ parseCookies $ fromMaybe "" $ getFromHeader headers "Cookie"
        ckey = AppKey . B.unpack $ fromMaybe "" $ getFromHeader cookies "key"

        pkey = AppKey $ takeKeyFromPath pathname

        timestamp = getFromHeaderOrParam headers rawuri "X-REQUEST-TIME" "timestamp"
        ts = fromMaybe (0::Int64) $ readMaybe $ B.unpack timestamp
        tp = getFromHeaderOrParam headers rawuri "X-REQUEST-TYPE" "type"
        nonce = getFromHeaderOrParam headers rawuri "X-REQUEST-NONCE" "nonce"
        sign = getFromHeaderOrParam headers rawuri "X-REQUEST-SIGNATURE" "sign"
        method = "WSPROXY"

        rejectRequest :: B.ByteString -> IO ()
        rejectRequest bs = WS.rejectRequest pendingConn $ "{\"err\": \"" <> bs <> "\"}"

        fillKeyOnPath :: Show a => a -> App -> App
        fillKeyOnPath n app = app {isKeyOnPath = show n == show pkey}

        process :: Show a => a -> Maybe App -> IO ()
        process n Nothing = rejectRequest $ "APP " <> B.pack (show n) <> " is not found."
        process n (Just app@App{onlyProxy = True}) = runAction $ fillKeyOnPath n app
        process n (Just app) =
          case signSecretKey isOnPath (B.pack . show $ appSecret app) of
            Left e -> WS.rejectRequest pendingConn $ "{\"err\": \"" <> B.pack e <> ".\"}"
            Right secret -> do
              now <- getEpochTime
              if verifyTime now then
                if verifySign (appKey app) secret
                   then runAction app'
                   else rejectRequest "Invalid REQUEST SIGNATURE"
              else rejectRequest "SIGNATURE TIMEOUT"

          where app' = fillKeyOnPath n app
                isOnPath = isKeyOnPath app'

        withDomainOr ::  IO () -> IO ()
        withDomainOr tryNext = do
          valid <- isValidDomain host
          if valid then process host =<< getAppByDomain host
                   else tryNext

        withKeyOr :: AppKey -> IO () -> IO ()
        withKeyOr k tryNext = do
          valid <- isValidKey key
          if valid then process k =<< liftIO (getAppByKey k)
                   else tryNext

        verifySign :: AppKey -> B.ByteString -> Bool
        verifySign rkey secret = equalSign exceptSign
          where exceptSign = signRaw secret
                  [ ("key", B.pack $ show rkey)
                  , ("timestamp", timestamp)
                  , ("pathname", t2b pathname)
                  ]

        equalSign :: CI B.ByteString -> Bool
        equalSign except = except == mk sign

        verifyTime :: Int64 -> Bool
        verifyTime now = now - 300 < ts

        signSecretKey :: Bool -> B.ByteString -> Either String B.ByteString
        signSecretKey isOnPath secret =
          case tp of
            "JSAPI" ->
              if B.null nonce
                then
                  Left "Invalid REQUEST NONCE"
                else
                  Right
                    . original
                    . hmacSHA256 nonce
                    $ B.concat
                      [ secret
                      , method
                      , t2b $ dropKeyFromPath isOnPath pathname
                      , timestamp
                      ]

            _ -> Right secret


        runAction :: App -> IO ()
        runAction app = do
          conn <- WS.acceptRequest pendingConn
          readChan  <- newTChanIO
          writeChan <- newTChanIO
          threads <- newTVarIO []
          let addThread t = atomically $ do
                xs <- readTVar threads
                writeTVar threads (t:xs)
              killThreads = do
                xs <- readTVarIO threads
                void . forkIO $ mapM_ killThread xs

          thread1 <- forkIO $ forever $ do
            bs <- atomically $ readTChan writeChan
            WS.sendDataMessage conn bs

          addThread thread1

          thread2 <- forkIO $ WS.pingThread conn 30 (return ())
          addThread thread2

          thread3 <- forkIO $ forever $ do
            bs0 <- try $ WS.receiveDataMessage conn
            case bs0 of
              Left (_ :: SomeException) -> killThreads
              Right bs1 -> atomically $ writeTChan readChan bs1

          addThread thread3

          prepareWsRequest app $ \h p -> do
            WS.runClientWith h p rawuri' WS.defaultConnectionOptions (removeFromHeader "Host" headers) $ \pconn -> do
              thread4 <- forkIO $ forever $ do
                bs <- atomically $ readTChan readChan
                WS.sendDataMessage pconn bs

              addThread thread4

              thread5 <- forkIO $ WS.pingThread pconn 30 (return ())

              addThread thread5

              thread6 <- myThreadId
              addThread thread6

              forever $ do
                bs0 <- try $ WS.receiveDataMessage pconn
                case bs0 of
                  Left (_ :: SomeException) -> killThreads
                  Right bs1 -> atomically $ writeTChan writeChan bs1

          where rawuri' = LT.unpack
                        $ dropKeyFromPath (isKeyOnPath app) (b2t rawuri)
