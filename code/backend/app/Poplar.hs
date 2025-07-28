module Poplar where

import GHC.Generics
import Data.Aeson

import Tree
import Participant
import State

data Poplar = Poplar {
  tree :: Tree,
  participants :: [Participant],
  nextId :: Int
  } deriving (Show, Generic)

instance ToJSON Poplar where
  toEncoding = genericToEncoding defaultOptions

emptyPoplar :: Poplar
emptyPoplar = Poplar {
  tree = emptyTree (StateId 1),
  participants = [],
  nextId = 2
  }

incrementId :: Poplar -> (Int, Poplar)
incrementId poplar =
  let myId = nextId poplar
  in (myId, poplar {nextId = myId + 1})
