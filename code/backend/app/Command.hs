module Command where

import Poplar
import Participant
import Credits
import Measurement


executeCommands :: [Poplar -> Poplar] -> Poplar -> Poplar
executeCommands commands poplar = foldl (\p command -> command p) poplar commands

addParticipant :: String -> Poplar -> Poplar
addParticipant userName oldPoplar =
  let newParticipantId = nextId oldPoplar
      newParticipant =
        Participant {
        participantId = ParticipantId newParticipantId,
        Participant.name = userName,
        creditBalance = CreditsAmount 0,
        preferenceSpec = Nothing,
        measurementDefinitions = []
        }
  in oldPoplar {
    nextId = 1 + nextId oldPoplar,
    participants = newParticipant : participants oldPoplar
    }

defineNewMeasurement :: ParticipantId -> String -> String -> Poplar -> Poplar
defineNewMeasurement definerId measurementName measurementDescription poplar =
    let newDefinitionId = nextId poplar
        newMeasurementDefinition = MeasurementDefinition {
          measurementDefinitionId = MeasurementDefinitionId newDefinitionId,
          Measurement.name = measurementName,
          description = measurementDescription
          }
        updatedParticipants =
          map (\participant ->
                  if participantId participant == definerId
                  then addDefinitionToParticipant newMeasurementDefinition participant
                  else participant)
          (participants poplar)
  in poplar {
      nextId = 1 + nextId poplar,
      participants = updatedParticipants
      }

setUserPreferenceToSpecificValue :: ParticipantId -> MeasurementDefinitionId -> Int -> Poplar -> Poplar
setUserPreferenceToSpecificValue participantId' measurementDefinitionId' requiredValue poplar =
  let updatedParticipants =
        map (\participant ->
                if participantId participant == participantId'
                then Participant.setUserPreferenceToSpecificValue measurementDefinitionId' requiredValue participant
                else participant)
        (participants poplar)
  in
    poplar {
    participants = updatedParticipants
    }
