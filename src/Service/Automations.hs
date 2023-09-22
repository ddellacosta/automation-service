module Service.Automations
  ( findAutomation
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Service.App (Logger, MonadMQTT)
import Service.Automation (Automation, nullAutomation)
import Service.AutomationName (AutomationName (..))
import Service.Automations.Gold (goldAutomation)
import Service.Automations.HTTP (httpAutomation)
import Service.Automations.HTTPDefault (defaultHttpAutomation)
import Service.Automations.LuaScript (luaAutomation)
import Service.Automations.StateManager (stateManagerAutomation)
import Service.Env (Env)

findAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => AutomationName
  -> (UTCTime -> Automation m)
findAutomation = \case
  Null               -> nullAutomation
  Gold               -> goldAutomation
  HTTPDefault        -> defaultHttpAutomation
  HTTP port          -> httpAutomation port
  LuaScript filePath -> luaAutomation filePath
  StateManager       -> stateManagerAutomation
