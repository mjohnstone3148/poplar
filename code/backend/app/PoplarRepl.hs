module PoplarRepl where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Encode.Pretty

import Poplar
import Command
import Query
import Measurement
import Time
import Tree
import Credits

examplePoplar :: Poplar
examplePoplar =
  let
    initialState  = emptyPoplar
    afterAddingUsers = executeCommands [
      addParticipant "Alice",
      addParticipant "Bob"
      ] initialState
    alice = assumeSuccess $ getParticipantIdByName "Alice" afterAddingUsers
    afterAddingXMeasurement = defineNewMeasurement alice "X" "The only value we care about" afterAddingUsers
    xMeasurementId = head $ getMeasurementDefinitionIds afterAddingXMeasurement

    -- Alice wants the value to be 2x5x7x11x13 = 10,010
    state3 = executeCommands [
      setUserPreferenceToSpecificValue alice xMeasurementId 10010,
      submitMeasurementForToday alice xMeasurementId (Value 1),
      addActionNode alice RootNodeRef "Multiply X by 2",
      addStateNode alice RootNodeRef (Day 2) [
          Measurement {definitionId = xMeasurementId, value = Value 2}
          ],
      addPaymentNode alice RootNodeRef (CreditsAmount 50)
      ] afterAddingXMeasurement
  in state3

assumeSuccess :: Maybe a -> a
assumeSuccess (Just x) = x
assumeSuccess Nothing = error "An operation which was expected to succeed actually failed"

prettyPrintPoplar :: Poplar -> IO ()
prettyPrintPoplar poplar = do
  let json = encodePretty poplar
      unpacked = LBS.unpack json
  putStrLn unpacked
