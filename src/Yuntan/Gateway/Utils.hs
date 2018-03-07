module Yuntan.Gateway.Utils
  (
    getEpochTime
  , b2t
  , t2b
  , flip'
  , takeKeyFromPath
  , dropKeyFromPath
  ) where

import qualified Data.ByteString    as B (ByteString)
import           Data.Int           (Int64)
import           Data.String        (IsString, fromString)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy     as LT (Text, fromStrict, toStrict)
import           Data.UnixTime
import           Foreign.C.Types    (CTime (..))

getEpochTime :: Num a => IO a
getEpochTime = fromIntegral . un . toEpochTime <$> getUnixTime
  where un :: CTime -> Int64
        un (CTime t) = t

b2t :: B.ByteString -> LT.Text
b2t = LT.fromStrict . decodeUtf8

t2b :: LT.Text -> B.ByteString
t2b = encodeUtf8 . LT.toStrict

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f c a b = f a b c

takeKeyFromPath :: String -> String
takeKeyFromPath path = takeWhile (/= '/') (drop 1 path)

dropKeyFromPath :: IsString a => Bool -> String -> a
dropKeyFromPath True path  = fromString $ dropWhile (/= '/') (drop 1 path)
dropKeyFromPath False path = fromString path
