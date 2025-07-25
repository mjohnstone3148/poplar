module Payment where

import GHC.Generics
import Data.Aeson

import Credits
import Participant

data Payment = Payment {
  toParticipant :: ParticipantId,
  amount :: CreditsAmount
  } deriving (Show, Generic)

instance ToJSON Payment where
  toEncoding = genericToEncoding defaultOptions
