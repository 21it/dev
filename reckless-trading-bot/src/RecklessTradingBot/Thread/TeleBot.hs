{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.TeleBot
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Chart as Chart
import Data.Maybe
import RecklessTradingBot.Import
import Telegram.Bot.API
import qualified Telegram.Bot.API.Methods as Api
import qualified Telegram.Bot.API.Types as Api
import Telegram.Bot.Simple

newtype TeleState = TeleState
  { unTeleState :: Maybe Bfx.Mma
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic
    )

newtype TeleAction = TeleAction
  { unTeleAction :: Bfx.Mma
  }
  deriving newtype
    ( Eq,
      Ord,
      NFData
    )
  deriving stock
    ( Generic
    )

instance GetAction (Maybe Bfx.Mma) TeleAction where
  getNextAction =
    (TeleAction <<$>>)

updateToAction :: Update -> TeleState -> Maybe TeleAction
updateToAction _ _ =
  Nothing

handleAction ::
  TeleAction ->
  TeleState ->
  Eff TeleAction TeleState
handleAction action _ =
  case action of
    TeleAction mma ->
      TeleState (Just mma) <# pure ()

teleJob ::
  ( Env m
  ) =>
  UnliftIO m ->
  TeleState ->
  Eff TeleAction TeleState
teleJob (UnliftIO run) prv = do
  prv <# do
    new <- liftIO $ run getLastMma
    chat <- liftIO $ teleEnvChat <$> run getTeleEnv
    whenJust new $ \mma ->
      when (new /= unTeleState prv)
        . void
        . liftClientM
        . Chart.withMmaPng mma
        $ \img ->
          Api.sendPhoto $
            Api.SendPhotoRequest
              { Api.sendPhotoChatId =
                  SomeChatUsername $
                    unTeleChat chat,
                Api.sendPhotoPhoto =
                  Api.MakePhotoFile $
                    Api.InputFile img "image/png",
                Api.sendPhotoThumb = Nothing,
                Api.sendPhotoCaption = Nothing,
                Api.sendPhotoParseMode = Nothing,
                Api.sendPhotoCaptionEntities = Nothing,
                Api.sendPhotoDisableNotification = Nothing,
                Api.sendPhotoProtectContent = Nothing,
                Api.sendPhotoReplyToMessageId = Nothing,
                Api.sendPhotoAllowSendingWithoutReply = Nothing,
                Api.sendPhotoReplyMarkup = Nothing
              }
    pure new

teleBot ::
  ( Env m
  ) =>
  UnliftIO m ->
  BotApp TeleState TeleAction
teleBot run =
  BotApp
    { botInitialModel = TeleState Nothing,
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs =
        [ BotJob
            { botJobSchedule = "* * * * *",
              botJobTask = teleJob run
            }
        ]
    }

apply :: (Env m) => m ()
apply = do
  $(logTM) DebugS "Spawned"
  teleEnv <- getTeleEnv
  teleClientEnv <-
    liftIO
      . defaultTelegramClientEnv
      . coerce
      $ teleEnvKey teleEnv
  withUnliftIO $ \run ->
    startBot_ (teleBot run) teleClientEnv
