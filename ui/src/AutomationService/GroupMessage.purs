module AutomationService.GroupMessage
  ( Message(..)
  )
where

import AutomationService.Group (Group)

data Message
  = LoadGroupsFailed String
  | LoadGroups (Array Group)
  | PublishGroupMsg String
