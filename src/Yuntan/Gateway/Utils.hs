module Yuntan.Gateway.Utils
  (
    getEpochTime
  , b2t
  , t2b
  , flip'
  ) where

import qualified Data.ByteString    as B (ByteString)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy     as LT (Text, fromStrict, toStrict)
import           Data.UnixTime

getEpochTime :: Read a => IO a
getEpochTime = read . show . toEpochTime <$> getUnixTime

b2t :: B.ByteString -> LT.Text
b2t = LT.fromStrict . decodeUtf8

t2b :: LT.Text -> B.ByteString
t2b = encodeUtf8 . LT.toStrict

flip' :: (a -> b -> c -> d) -> c -> a -> b -> d
flip' f c a b = f a b c