module Rhino.Utils where

import Rhino.Prelude

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map

foldM1 :: Monad m => (a -> a -> m a) -> NonEmpty a -> m a
foldM1 f (a :| as) = foldM f a as

-- | A version of 'Map.lookup' that panics if the key is missing
lookupChecked :: (Show k, Ord k, HasCallStack) => k -> Map k v -> v
lookupChecked k m = fromMaybe (panic msg) $ Map.lookup k m
  where
    msg = "key: " <> show k

-- | A version of 'zip' that panics if the two lists have different lengths
zipEven :: HasCallStack => [a] -> [b] -> [(a, b)]
zipEven as bs
  | length as /= length bs = oops
  | otherwise = zip as bs

duplicates :: Ord a => [a] -> [a]
duplicates as = ordNub (as \\ ordNub as)
