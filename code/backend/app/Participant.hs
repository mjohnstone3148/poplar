module Participant where

import GHC.Generics
import Data.Aeson

import Credits
import Preference
import Measurement

data Participant = Participant {
  participantId :: ParticipantId,
  name :: String,
  creditBalance :: CreditsAmount,
  preferenceSpec :: Maybe PreferenceSpec,
  measurementDefinitions :: [MeasurementDefinition]
  } deriving (Show, Generic)

data ParticipantId = ParticipantId Int
                   deriving (Eq, Show, Generic)

instance ToJSON Participant where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ParticipantId where
  toEncoding = genericToEncoding defaultOptions

addDefinitionToParticipant :: MeasurementDefinition -> Participant -> Participant
addDefinitionToParticipant newMeasurementDefinition participant =
  participant {measurementDefinitions = newMeasurementDefinition : measurementDefinitions participant}

setUserPreferenceToSpecificValue :: MeasurementDefinitionId -> Int -> Participant -> Participant
setUserPreferenceToSpecificValue measurementDefinitionId' requiredValue participant =
  let newSpec = RequireExactValuePreference measurementDefinitionId' requiredValue
  in participant {preferenceSpec = Just newSpec}
