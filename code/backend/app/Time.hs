module Time where

import GHC.Generics
import Data.Aeson

data Day = Day Int
         deriving (Show, Generic)

instance ToJSON Day where
  toEncoding = genericToEncoding defaultOptions
