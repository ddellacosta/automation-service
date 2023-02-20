module Service.Actions
  ( findAction
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.App (Logger, MonadMQTT)
import Service.Action (Action, nullAction)
import Service.ActionName (ActionName(..))
import Service.Actions.Chrizmaz (chrizmazAction)
import Service.Actions.Gold (goldAction)
import Service.Actions.Trinity (trinityAction)
import Service.Env (Env')

findAction
  :: (Logger m, MonadMQTT m, MonadReader (Env' logger mqttClient) m, MonadUnliftIO m)
  => ActionName
  -> Action m
findAction = \case
  Null -> nullAction
  Gold -> goldAction
  Chrizmaz -> chrizmazAction
  Trinity -> trinityAction
  -- ...shouldn't get here because this isn't dispatched using
  -- findAction. I could probably use the type system in a more
  -- sophisticated way to implement this but, meh, for now at least
  LuaScript -> nullAction
