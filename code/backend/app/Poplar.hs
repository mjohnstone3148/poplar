module Poplar where

import Tree
import Participant

data Poplar = Poplar {
  tree :: Tree,
  participants :: [Participant],
  nextId :: Int
  }
