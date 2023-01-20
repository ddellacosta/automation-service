module Service.Actions (findAction) where

import Data.UUID (UUID)
import Service.Action (Action, Message, nullAction)
import Service.ActionName (ActionName(..))
import Service.Actions.Chrizmaz (chrizmazAction)
import Service.Actions.Gold (goldAction)
import Service.Actions.Trinity (trinityAction)
import UnliftIO.STM (TChan)

findAction :: ActionName -> (UUID -> Action (TChan Message))
findAction = \case
  Gold -> goldAction
  Chrizmaz -> chrizmazAction
  Trinity -> trinityAction
  _ -> nullAction
