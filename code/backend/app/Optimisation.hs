module Optimisation where

import Tree
import Time
import Payment
import Action

findOptimalPath :: Tree -> [PathElement]
findOptimalPath = error "findOptimalPath nyi"

data PathElement = MakePayment Day Payment
                 | PerformAction Day Action
