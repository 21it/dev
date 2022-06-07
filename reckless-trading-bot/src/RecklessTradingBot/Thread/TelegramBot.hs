{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Thread.TelegramBot
  ( apply,
  )
where

import qualified BitfinexClient as Bfx
import qualified BitfinexClient.Chart as Chart
import Data.Maybe
import RecklessTradingBot.Import
import Telegram.Bot.API
import Telegram.Bot.Simple
import qualified Telegram.Bot.Simple.Reply as Reply

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
    whenJust new $ \mma ->
      when (new /= unTeleState prv) $
        Reply.replyTo
          (SomeChatUsername "TODO")
          . Reply.toReplyMessage
          . Chart.unMmaHeader
          $ Chart.newMmaHeader Bfx.Ctf1m mma
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
      $ unTeleEnv teleEnv
  withUnliftIO $ \run ->
    startBot_ (teleBot run) teleClientEnv
