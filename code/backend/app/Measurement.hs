module Measurement where

import GHC.Generics
import Data.Aeson hiding (Value)

instance ToJSON MeasurementDefinitionId where
  toEncoding = genericToEncoding defaultOptions

data MeasurementDefinition =  MeasurementDefinition {
  measurementDefinitionId ::  MeasurementDefinitionId,
  name :: String,
  description :: String
  } deriving (Show, Generic)


instance ToJSON MeasurementDefinition where
  toEncoding = genericToEncoding defaultOptions
  
data MeasurementDefinitionId =  MeasurementDefinitionId Int
                             deriving (Eq, Show, Generic)

data Measurement = Measurement {
  definitionId :: MeasurementDefinitionId,
  value :: Value
  } deriving (Show, Generic)

data Value = Value Int
           deriving (Eq, Show, Generic)

instance ToJSON Measurement where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Measurement.Value where
  toEncoding = genericToEncoding defaultOptions
