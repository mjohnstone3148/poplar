module Action where

import Participant

data Action = Action {
  actionId :: ActionId,
  createdBy :: ParticipantId,
  description :: String
}

data ActionId = ActionId Int
