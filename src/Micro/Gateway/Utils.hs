module Micro.Gateway.Utils
  ( getEpochTime
  , b2t
  , t2b
  , flip'
  , takeKeyFromPath
  , dropKeyFromPath
  ) where

import qualified Data.ByteString    as B (ByteString)
import           Data.Int           (Int64)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy     as LT (Text, drop, dropWhile, fromStrict,
                                           takeWhile, toStrict)
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

takeKeyFromPath :: LT.Text -> LT.Text
takeKeyFromPath = LT.takeWhile (/= '/') . LT.drop 1

dropKeyFromPath :: Bool -> LT.Text -> LT.Text
dropKeyFromPath True  = LT.dropWhile (/= '/') . LT.drop 1
dropKeyFromPath False = id
