module Preference where

import GHC.Generics
import Data.Aeson

import Measurement
import State

type ParticipantPreference = State -> Bool

data PreferenceSpec = RequireExactValuePreference MeasurementDefinitionId Int
                    deriving (Show, Generic)

instance ToJSON PreferenceSpec where
  toEncoding = genericToEncoding defaultOptions

makePreferenceFunction :: PreferenceSpec -> ParticipantPreference
makePreferenceFunction (RequireExactValuePreference measurementDefinitionId' valueRequired) state =
  let matchingMeasurements = filter
        (\m -> definitionId m == measurementDefinitionId')
        (measurements state)
  in case matchingMeasurements of
    [measurement] -> value measurement == (Value valueRequired)
    _ -> False
