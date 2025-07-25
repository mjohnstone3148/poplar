module Prediction where

import Credits

data CreditAllocation = CreditAllocation {
  creditsAllocated :: CreditsAmount,
  direction :: Direction
  }

data Direction = For | Against
