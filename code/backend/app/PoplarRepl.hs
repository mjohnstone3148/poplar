module PoplarRepl where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson.Encode.Pretty

import Poplar
import Command
import Query

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
    state3 = setUserPreferenceToSpecificValue alice xMeasurementId 10010 afterAddingXMeasurement 
  
  in state3

assumeSuccess :: Maybe a -> a
assumeSuccess (Just x) = x
assumeSuccess Nothing = error "An operation which was expected to succeed actually failed"

prettyPrintPoplar :: Poplar -> IO ()
prettyPrintPoplar poplar = do
  let json = encodePretty poplar
      unpacked = LBS.unpack json
  putStrLn unpacked
