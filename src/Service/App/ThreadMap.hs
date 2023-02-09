module Service.App.ThreadMap
  ( ActionEntry
  , ThreadMap
  , insertAction
  )
  where

import qualified Data.Map.Strict as M
import Service.Action (Action)
import Service.ActionName (ActionName)
import UnliftIO.Async (Async)
import UnliftIO.STM (STM, TVar, readTVar, writeTVar)

type ActionEntry m = (Action m, Async ())

type ThreadMap m = M.Map ActionName [ActionEntry m]

-- |
-- | Given a TVar ThreadMap and a (ActionName, (Action m, Async ()))
-- | pair, inserts a new entry into the ThreadMap. If the ThreadMap
-- | already contains a List of (Action m, Async ()) pairs at that
-- | index it will append a new one to the end of that List, otherwise
-- | it will add a new List with the new entry as its first member.
-- |
insertAction :: TVar (ThreadMap m) -> ActionName -> ActionEntry m -> STM ()
insertAction threadMap actionName actionEntry = do
  threadMap' <- readTVar threadMap
  writeTVar threadMap $
    M.alter
      (\case
          Nothing -> Just [actionEntry]
          Just actions -> Just (actions <> [actionEntry]))
      actionName
      threadMap'
