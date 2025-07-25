module Action where

import GHC.Generics
import Data.Aeson

import Participant

data Action = Action {
  actionId :: ActionId,
  createdBy :: ParticipantId,
  description :: String
} deriving (Show, Generic)

instance ToJSON Action where
  toEncoding = genericToEncoding defaultOptions

data ActionId = ActionId Int
              deriving (Show, Generic)

instance ToJSON ActionId where
  toEncoding = genericToEncoding defaultOptions
