module Tree where

import GHC.Generics
import Data.Aeson

import Time
import Payment
import State
import Action

data Tree = Tree {
  rootNode :: TreeNode
  } deriving (Show, Generic)

data TreeNode = StateNode Day State [TreeNode]
              | PaymentNode Payment [TreeNode]
              | ActionNode Action [TreeNode]
              deriving (Show, Generic)

instance ToJSON Tree where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TreeNode where
  toEncoding = genericToEncoding defaultOptions

emptyTree :: StateId -> Tree
emptyTree rootStateId = Tree {
  rootNode = StateNode (Day 1) (State {stateId = rootStateId, measurements = []}) []
  }
