module Payment where

import GHC.Generics
import Data.Aeson

import Credits
import Participant

data Payment = Payment {
  paymentId :: PaymentId,
  toParticipant :: ParticipantId,
  amount :: CreditsAmount
  } deriving (Show, Generic)

data PaymentId = PaymentId Int
               deriving (Show, Generic)

instance ToJSON Payment where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON PaymentId where
  toEncoding = genericToEncoding defaultOptions
