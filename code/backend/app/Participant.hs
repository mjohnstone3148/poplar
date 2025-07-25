module Participant where

import Credits

data Participant = Participant {
  participantId :: ParticipantId,
  name :: String,
  creditBalance :: CreditsAmount
  }

data ParticipantId = ParticipantId Int

