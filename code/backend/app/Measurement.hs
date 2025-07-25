module Measurement where

import Participant

data MeasurementDefinition =  MeasurementDefinition {
  measurementDefinitionId ::  MeasurementDefinitionId,
  createdBy :: ParticipantId,
  name :: String,
  description :: String
  }
  
data MeasurementDefinitionId =  MeasurementDefinitionId Int


data Measurement = Measurement {
  definitionId :: MeasurementDefinitionId,
  value :: Value
  }

data Value = Value Int
