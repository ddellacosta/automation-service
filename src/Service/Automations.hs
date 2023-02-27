module Service.Automations
  ( findAutomation
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.App (Logger, MonadMQTT)
import Service.Automation (Automation, nullAutomation)
import Service.AutomationName (AutomationName(..))
import Service.Automations.Chrizmaz (chrizmazAutomation)
import Service.Automations.Gold (goldAutomation)
import Service.Automations.LuaScript (luaAutomation)
import Service.Automations.OnLow (onLowAutomation)
import Service.Automations.Trinity (trinityAutomation)
import Service.Env (Env)

findAutomation
  :: (Logger m, MonadMQTT m, MonadReader Env m, MonadUnliftIO m)
  => AutomationName
  -> Automation m
findAutomation = \case
  Null -> nullAutomation
  Gold -> goldAutomation
  Chrizmaz -> chrizmazAutomation
  Trinity -> trinityAutomation
  LuaScript filePath -> luaAutomation filePath
  OnLow -> onLowAutomation
