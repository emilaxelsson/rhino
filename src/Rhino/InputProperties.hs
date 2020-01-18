module Rhino.InputProperties
  ( InputPropsError (..)
  , inputProperties
  ) where

import Rhino.Prelude

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map

import Data.DAG
import Rhino.AST

data InputPropsError
  = IncoherentInputProperties Identifier Input Input
  deriving (Show)

instance Exception InputPropsError

type Properties = Map Identifier (NonEmpty Input)

coherentMerge ::
     Identifier
  -> NonEmpty Input
  -> NonEmpty Input
  -> Either InputPropsError (NonEmpty Input)
coherentMerge i = go
  where
    go (a :| as) (b :| bs)
      | a == b = case (nonEmpty as, nonEmpty bs) of
          (Nothing, _) -> return (b :| bs)
          (_, Nothing) -> return (a :| as)
          (Just as', Just bs') -> (pure a <>) <$> go as' bs'
      | otherwise = Left $ IncoherentInputProperties i a b

coherentMergeList
  :: Identifier -- ^ Input name
  -> NonEmpty (NonEmpty Input) -- ^ Alternative sequences of declarations
  -> Either InputPropsError (NonEmpty Input)
coherentMergeList i (a :| as) = go a as
  where
    go c [] = return c
    go c (d:ds) = do
      cd <- coherentMerge i c d
      go cd ds

-- | Merge properties from child modules
mergeChildren :: [Properties] -> Either InputPropsError Properties
mergeChildren =
  Map.traverseWithKey coherentMergeList . Map.unionsWith (<>) . map (fmap pure)

declare :: Properties -> Declaration -> Properties
declare props (InputDecl inp) =
  Map.insertWith (<>) (inputName inp) (pure inp) props
declare props (DefDecl _) = props

inputProperties' :: DAG ModulePath Module -> Either InputPropsError Properties
inputProperties' = foldDagM $ \childProps Module {..} -> do
  mergedChildren <- mergeChildren childProps
  return $ foldl' declare mergedChildren program

inputProperties ::
     DAG ModulePath Module -> Either InputPropsError (Map Identifier Input)
inputProperties = fmap (fmap NonEmpty.head) . inputProperties'
