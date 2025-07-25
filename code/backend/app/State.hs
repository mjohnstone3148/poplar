module State where

import Measurement

data State = State {
  stateId :: StateId,
  measurements :: [Measurement]
  }

data StateId = StateId Int
