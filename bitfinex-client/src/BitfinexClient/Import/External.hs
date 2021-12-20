{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Import.External
  ( module X,
  )
where

import Control.Monad.Trans.Except as X (except, throwE)
import Data.Aeson as X
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
  )
import Data.ByteString.Lazy as X (ByteString)
import Data.Coerce as X (coerce)
import Data.Fixed as X (Fixed, HasResolution (..), showFixed)
import Data.Metrology.Poly as X
  ( quOf,
    (*|),
    (/|),
    (:/),
    (|*),
    (|*|),
    (|+|),
    (|-|),
    (|/),
    (|/|),
  )
import Data.Ratio as X ((%))
import Data.Ratio.Rounding as X (dpRound, sdRound)
import Data.Singletons as X
import Data.Time.Clock as X
  ( DiffTime,
    UTCTime (..),
    addUTCTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    getCurrentTime,
    secondsToDiffTime,
  )
import Data.Type.Equality as X
  ( TestEquality (..),
    (:~:) (..),
  )
import Database.Persist as X
  ( PersistField (..),
    PersistValue (..),
    SqlType (..),
  )
import Database.Persist.Sql as X
  ( PersistFieldSql (..),
  )
import Network.HTTP.Client as X (HttpException (..))
import Universum as X hiding (ByteString, catch)
import UnliftIO as X (catch)
import Witch as X
  ( From (..),
    TryFrom (..),
    TryFromException (..),
    composeTry,
    composeTryLhs,
    composeTryRhs,
    into,
    tryVia,
    via,
    withSource,
    withTarget,
  )
