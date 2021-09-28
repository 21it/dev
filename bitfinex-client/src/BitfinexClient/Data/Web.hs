{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Web
  ( PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    RawResponse (..),
    Nonce,
    unNonce,
    newNonce,
    epoch,
  )
where

import BitfinexClient.Import.External
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Prelude

newtype PrvKey
  = PrvKey BS.ByteString
  deriving newtype (Eq, Ord, IsString)

instance Prelude.Show PrvKey where
  show = const "SECRET"

newtype ApiKey
  = ApiKey BS.ByteString
  deriving newtype (Eq, Ord, IsString)

instance Prelude.Show ApiKey where
  show = const "SECRET"

data RequestMethod
  = GET
  | POST
  deriving stock (Eq, Ord, Show)

newtype BaseUrl
  = BaseUrl Text
  deriving newtype (Eq, Ord, Show, IsString)

newtype RawResponse
  = RawResponse ByteString
  deriving newtype (Eq, Ord)

instance Show RawResponse where
  show x =
    case decodeUtf8' bs of
      Left {} -> "ByteString RawResponse" <> show (BS.unpack bs)
      Right res -> "Text RawResponse " <> T.unpack res
    where
      bs = BL.toStrict $ coerce x

newtype Nonce = Nonce {unNonce :: Natural}
  deriving newtype (Eq, Ord, Show)

newNonce :: MonadIO m => m Nonce
newNonce = liftIO $ Nonce . utcTimeToMicros <$> getCurrentTime

utcTimeToMicros :: UTCTime -> Natural
utcTimeToMicros x =
  fromInteger $
    diffTimeToPicoseconds
      ( fromRational
          . toRational
          $ diffUTCTime x epoch
      )
      `div` 1000000

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
