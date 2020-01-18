module Rhino.Prelude
  ( module X
  , module Rhino.Prelude
  ) where

import Prelude as X (Show)

import CorePrelude as X hiding (first, second, undefined)

import Protolude.CallStack as X
import Protolude.Conv as X
import Protolude.Show as X (putText)
import Protolude as X
  ( callStack
  , headMay
  , ordNub
  , readFile
  , show
  , trace
  , traceM
  , traceShow
  , undefined
  )

import Control.Monad as X
import Data.Bifunctor as X
import Data.Coerce as X
import Data.Foldable as X
import Data.List as X
  ( (++)
  , (\\)
  , drop
  , dropWhile
  , lines
  , map
  , repeat
  , replicate
  , sort
  , take
  , takeWhile
  , unlines
  , zip
  )
import Data.List.NonEmpty as X (NonEmpty)
import Data.Traversable as X
import Data.Void as X

-- For local use:
import Control.Exception (throw)
import GHC.Stack (CallStack, prettyCallStack)
import qualified Prelude

data RhinoInternalError = RhinoInternalError
  { rhinoInternalErrorStack :: CallStack
  , rhinoInternalErrorMessage :: Text
  }

instance Show RhinoInternalError where
  show RhinoInternalError {..} = Prelude.unlines
    [ prettyCallStack rhinoInternalErrorStack
    , "  Error message:"
    , Prelude.unlines $
      map ("    " ++) $ Prelude.lines $ toS rhinoInternalErrorMessage
    ]

instance Exception RhinoInternalError

-- For some reason 'panic' in Protolude doesn't use the call stack. Otherwise,
-- we'd use that instead.

panic :: HasCallStack => Text -> a
panic t = throw $ RhinoInternalError callStack t

oops :: HasCallStack => a
oops = panic "oops: should not happen"
