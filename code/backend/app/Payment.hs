module Payment where

import Credits
import Participant

data Payment = Payment {
  toParticipant :: ParticipantId,
  amount :: CreditsAmount
  }
