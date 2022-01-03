{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.Web
  ( PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    RawResponse (..),
    Nonce,
    unNonce,
    NonceGen,
    newNonceGen,
    withNonce,
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
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance Prelude.Show PrvKey where
  show =
    const "SECRET"

newtype ApiKey
  = ApiKey BS.ByteString
  deriving newtype
    ( Eq,
      Ord,
      IsString
    )

instance Prelude.Show ApiKey where
  show =
    const "SECRET"

data RequestMethod
  = GET
  | POST
  deriving stock
    ( Eq,
      Ord,
      Show
    )

newtype BaseUrl
  = BaseUrl Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      IsString
    )

newtype RawResponse
  = RawResponse ByteString
  deriving newtype
    ( Eq,
      Ord
    )

instance Show RawResponse where
  show x =
    case decodeUtf8' bs of
      Left {} -> "ByteString RawResponse" <> show (BS.unpack bs)
      Right res -> "Text RawResponse " <> T.unpack res
    where
      bs = BL.toStrict $ coerce x

newtype Nonce = Nonce
  { unNonce :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )

mkNonce :: (MonadIO m) => m Nonce
mkNonce =
  liftIO $
    Nonce . utcTimeToMicros <$> getCurrentTime

newtype NonceGen
  = NonceGen (MVar Nonce)
  deriving stock
    ( Eq
    )

instance Prelude.Show NonceGen where
  show =
    const "NonceGen"

newNonceGen ::
  ( MonadIO m
  ) =>
  m NonceGen
newNonceGen = do
  nonce <- mkNonce
  var <- liftIO $ newMVar nonce
  pure $ NonceGen var

withNonce ::
  ( MonadUnliftIO m
  ) =>
  NonceGen ->
  (Nonce -> m a) ->
  m a
withNonce (NonceGen var) this = do
  nextNonce <- mkNonce
  bracket
    (takeMVar var)
    (putMVar var . max nextNonce)
    (const $ this nextNonce)

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
epoch =
  posixSecondsToUTCTime 0
