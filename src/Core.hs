module Core where

import RIO.Map qualified as Map

data Build = Build
  { pipeline :: Int
  , bstate :: Int
  , completedSteps :: Map Int
  }
  deriving stock (Eq, Show)
