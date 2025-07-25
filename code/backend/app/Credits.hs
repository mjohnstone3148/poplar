module Credits where

import GHC.Generics
import Data.Aeson

data CreditsAmount = CreditsAmount Int
                   deriving (Show, Generic)

instance ToJSON CreditsAmount where
  toEncoding = genericToEncoding defaultOptions
