module Tree where

import Time
import Payment
import State
import Action

data Tree = Tree {
  rootNode :: TreeNode
  }

data TreeNode = StateNode Day State [TreeNode]
              | PaymentNode Payment [TreeNode]
              | ActionNode Action [TreeNode]
