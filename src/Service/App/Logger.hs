module Service.App.Logger
  ( Logger(..)
  )
where

import Data.Text (Text)
import Service.Env.Config (LogLevel (..))
import System.Log.FastLogger (TimedFastLogger, ToLogStr (..))

class Logger l where
  log :: l -> LogLevel -> Text -> IO ()

instance Logger TimedFastLogger where
  log logger' level logStr = logger' $ \time ->
    toLogStr (show level) <> " - " <> toLogStr time <> " - " <> toLogStr logStr <> "\n"
