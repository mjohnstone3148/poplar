module Query where

import Poplar
import Optimisation
import Participant
import Measurement

getOptimalPath :: Poplar -> [PathElement]
getOptimalPath = error "getOptimalPath nyi"

getParticipantIdByName :: String -> Poplar -> Maybe ParticipantId
getParticipantIdByName name' poplar =
  case filter (\p -> Participant.name p == name') (participants poplar) of
    [participant] -> Just $ participantId participant
    _ -> Nothing

getMeasurementDefinitionIds :: Poplar -> [MeasurementDefinitionId]
getMeasurementDefinitionIds poplar =
  map measurementDefinitionId $ concatMap measurementDefinitions (participants poplar)
