module State where

import GHC.Generics
import Data.Aeson

import Measurement

data State = State {
  stateId :: StateId,
  measurements :: [Measurement]
  } deriving (Show, Generic)

data StateId = StateId Int
             deriving (Show, Generic)

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions

  
instance ToJSON StateId where
  toEncoding = genericToEncoding defaultOptions
