module Rhino.Error where

import Rhino.Prelude

data Mismatch expected actual = Mismatch
  { expected :: expected
  , got :: actual
  } deriving (Eq, Show)

