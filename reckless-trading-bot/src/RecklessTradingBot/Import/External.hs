{-# OPTIONS_HADDOCK show-extensions #-}

module RecklessTradingBot.Import.External (module X) where

import BitfinexClient.Data.Metro as X
  ( moneyBaseBuy,
    moneyBaseSell,
    moneyQuoteBuy,
    moneyQuoteSell,
  )
import BitfinexClient.Data.QQ as X
import BitfinexClient.Import.Witch as X
import BitfinexClient.Util as X
import Control.Concurrent.Async as X (waitAnyCancel)
import Control.Concurrent.STM as X (atomically)
import Control.Concurrent.STM.TChan as X
  ( TChan,
    dupTChan,
    newBroadcastTChan,
    newTChan,
    readTChan,
    writeTChan,
  )
import Control.Concurrent.Thread.Delay as X (delay)
import Control.Error.Safe as X (tryJust)
import Control.Monad.Trans.Except as X
  ( except,
    throwE,
    withExceptT,
  )
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Coerce as X (coerce)
import Data.Either.Extra as X (fromEither)
import Data.Fixed as X (Pico)
import Data.List as X (partition)
import Data.Metrology.Poly as X
  ( (*|),
    (/|),
    (:/),
    (|*),
    (|*|),
    (|+|),
    (|-|),
    (|/),
    (|/|),
    (|<=|),
  )
import Data.Pool as X (Pool, destroyAllResources)
import Data.Ratio as X ((%))
import Data.Singletons as X
import Data.Time.Clock as X
  ( DiffTime,
    NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    secondsToDiffTime,
    secondsToNominalDiffTime,
  )
import Data.Type.Equality as X
  ( TestEquality (..),
    (:~:) (..),
    type (==),
  )
import Database.Esqueleto.Legacy as X
  ( Entity (..),
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (..),
    RawSql (..),
    SqlBackend,
    SqlPersistT,
    SqlType (..),
    getBy,
    insertBy,
    putMany,
    rawExecute,
    rawSql,
    runMigration,
    runSqlPool,
  )
import Database.Persist as X (selectList)
import Database.Persist.Postgresql as X
  ( ConnectionString,
    createPostgresqlPool,
  )
import Database.Persist.TH as X (derivePersistField)
import Katip as X
  ( ColorStrategy (..),
    Environment (..),
    Katip (..),
    KatipContext (..),
    KatipContextT,
    LogContexts,
    LogEnv,
    Namespace,
    Severity (..),
    Verbosity (..),
    bracketFormat,
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    jsonFormat,
    logStr,
    logTM,
    mkHandleScribeWithFormatter,
    permitItem,
    registerScribe,
    runKatipContextT,
  )
import Universum as X hiding
  ( atomically,
    bracket,
    on,
    set,
    (^.),
  )
import UnliftIO as X
  ( MonadUnliftIO,
    UnliftIO (..),
    bracket,
    withRunInIO,
    withUnliftIO,
  )
