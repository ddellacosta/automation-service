module Service.Actions (findAction) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.UUID (UUID)
import Service.App (Logger, MonadMQTT)
import Service.Action (Action, nullAction)
import Service.ActionName (ActionName(..))
import Service.Actions.Chrizmaz (chrizmazAction)
import Service.Actions.Gold (goldAction)
import Service.Actions.Trinity (trinityAction)

findAction :: (Logger m, MonadMQTT m, MonadUnliftIO m) => ActionName -> (UUID -> Action m)
findAction = \case
  Gold -> goldAction
  Chrizmaz -> chrizmazAction
  Trinity -> trinityAction
  _ -> nullAction
