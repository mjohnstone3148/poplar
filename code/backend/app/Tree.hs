module Tree where

import GHC.Generics
import Data.Aeson

import Time
import Payment
import State
import Action

data Tree = Tree {
  rootNode :: RootNode
  } deriving (Show, Generic)

data TreeNode = StateNode Day State [TreeNode]
              | PaymentNode Payment [TreeNode]
              | ActionNode Action [TreeNode]
              deriving (Show, Generic)

data RootNode = RootNode Day State [TreeNode]
              deriving (Show, Generic)

instance ToJSON Tree where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TreeNode where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON RootNode where
  toEncoding = genericToEncoding defaultOptions

data NodeReference = RootNodeRef
                   | NonRootNodeRef Int

emptyTree :: StateId -> Tree
emptyTree rootStateId = Tree {
  rootNode = RootNode (Day 1) (State {stateId = rootStateId, measurements = []}) []
  }
