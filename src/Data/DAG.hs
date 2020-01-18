module Data.DAG
  ( DagError (..)
  , HasChildren (..)
  , Annotated (..)
  , DAG (..)
  , printDAG
  , annotatedDAG
  , unAnnnotateDAG
  , singleton
  , mkDag'
  , mkDag
  , traverseDag
  , traverseDagM
  , mapAnnotated
  , mapAnnotatedM
  , foldDag
  , foldDagM
  , checkIncompleteDAG
  ) where

import Rhino.Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State
  ( State
  , StateT
  , evalState
  , get
  , modify
  , runStateT
  )
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Text as Text

import Rhino.Utils

data DagError node nodeId
  = Cycle [nodeId]
  | DanglingEdge node nodeId -- from to
  | DuplicateNodes [nodeId]
  deriving (Show)

class HasChildren node where
  type NodeId node
  children :: node -> [NodeId node]

data Annotated a ann = Annotated
  { annotation :: ann
  , annotatee  :: a
  } deriving (Eq, Show, Functor)

instance HasChildren a => HasChildren (Annotated a ann) where
  type NodeId (Annotated a ann) = NodeId a
  children = children . annotatee

data DAG nodeId node = DAG
  { root  :: node
  , nodes :: Map nodeId node
  } deriving (Eq, Show)

printDAG :: (Show nodeId, Show node) => DAG nodeId node -> IO ()
printDAG DAG {..} = putStrLn $ Text.unlines $ concat
  [ [ "-- root ----------------"
    , show root
    , ""
    , "-- nodes ---------------"
    ]
  , [ show ni <> ": " <> show n
    | (ni, n) <- Map.toList nodes
    ]
  ]

-- | Uniformly annotate the DAG nodes with the given value
annotatedDAG :: ann -> DAG nodeId node -> DAG nodeId (Annotated node ann)
annotatedDAG ann DAG {..} = DAG
  { root = Annotated ann root
  , nodes = Annotated ann <$> nodes
  }

unAnnnotateDAG :: DAG nodeId (Annotated node ann) -> DAG nodeId node
unAnnnotateDAG DAG {..} = DAG
  { root = annotatee root
  , nodes = annotatee <$> nodes
  }

singleton :: node -> DAG nodeId node
singleton root = DAG root Map.empty

data VisitState
  = Visiting
  | Done
  deriving (Eq, Show)

takeCycle :: Eq a => NonEmpty a -> NonEmpty a
takeCycle (a :| as) = a :| takeWhile (/= a) as

cyclic ::
     (HasChildren node, Ord nodeId, nodeId ~ NodeId node)
  => node
  -> Map nodeId node
  -> ExceptT (DagError node nodeId) (State (Map nodeId VisitState)) ()
cyclic r ns = mapM_ (go' r []) $ children r
  where
    go path@(ni :| _) n = do
      st <- get
      case Map.lookup ni st of
        Nothing -> do
          modify $ Map.insert ni Visiting
          mapM_ (go' n $ toList path) cs
          modify $ Map.insert ni Done
        Just Visiting -> throwError $ Cycle $ toList $ takeCycle path
        Just Done -> return ()
      where
        cs = children n

    go' n path ni = case Map.lookup ni ns of
      Nothing -> throwError $ DanglingEdge n ni
      Just n' -> go (ni :| path) n'

mkDag' ::
     (HasChildren node, Ord nodeId, nodeId ~ NodeId node)
  => node
  -> Map nodeId node
  -> Either (DagError node nodeId) (DAG nodeId node)
mkDag' root nodes = do
  flip evalState Map.empty $ runExceptT $ cyclic root nodes
  return DAG {..}

mkDag ::
     (HasChildren node, Ord nodeId, nodeId ~ NodeId node)
  => node
  -> [(nodeId, node)]
  -> Either (DagError node nodeId) (DAG nodeId node)
mkDag root ns
  | not (null dups) = throwError $ DuplicateNodes dups
  | otherwise = do
    flip evalState Map.empty $ runExceptT $ cyclic root nodes
    return DAG {..}
  where
    dups = duplicates $ map fst ns
    nodes = Map.fromList ns

-- | Monadic bottom-up 'DAG' traversal
traverseDagM ::
     forall nodeId node m a.
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , Monad m
     , HasCallStack
     )
  => (  [a]  -- results for the current node's children
     -> node -- current node
     -> m a  -- result for current node
     )
  -> DAG nodeId node
  -> m (a, Map nodeId a)
       -- ^ (result for root, results for internal nodes)
traverseDagM f DAG {..} = flip runStateT Map.empty $ go Nothing root
  where
    go
      :: Maybe nodeId -- ^ Node id if there is one (the root has no id)
      -> node
      -> StateT (Map nodeId a) m a
    go ni n = do
      st <- get
      case ni >>= flip Map.lookup st of
        Just cached -> return cached
        Nothing -> do
          cs' <- forM (children n) $ \c -> go (Just c) $ lookupChecked c nodes
          a <- lift $ f cs' n
          forM_ ni $ modify . flip Map.insert a
            -- Cache the result if the node has an id
          return a

-- |  Bottom-up 'DAG' traversal
traverseDag ::
     forall nodeId node a.
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , HasCallStack
     )
  => (  [a]  -- results for the current node's children
     -> node -- current node
     -> a    -- result for current node
     )
  -> DAG nodeId node
  -> (a, Map nodeId a)
       -- ^ (result for root, results for internal nodes)
traverseDag f = runIdentity . traverseDagM (\as n -> pure $ f as n)

-- |  Bottom-up mapping of annotations in a 'DAG'
--
-- The structure of the 'DAG' is unchanged; only annotations are affected.
mapAnnotated ::
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , HasCallStack
     )
  => (  [ann2]              -- new annotations of children
     -> Annotated node ann1 -- current node
     -> ann2                -- new annotation
     )
  -> DAG nodeId (Annotated node ann1)
  -> DAG nodeId (Annotated node ann2)
mapAnnotated f dag =
  uncurry DAG $
  flip traverseDag dag $ \as n ->
    Annotated (f (map annotation as) n) (annotatee n)

-- | Monadic version of 'mapAnnotated'
mapAnnotatedM ::
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , Monad m
     , HasCallStack
     )
  => (  [ann2]              -- new annotations of children
     -> Annotated node ann1 -- current node
     -> m ann2              -- new annotation
     )
  -> DAG nodeId (Annotated node ann1)
  -> m (DAG nodeId (Annotated node ann2))
mapAnnotatedM f dag =
  fmap (uncurry DAG) $
  flip traverseDagM dag $ \as n ->
    Annotated <$> f (map annotation as) n <*> pure (annotatee n)

-- | Fold a dag
--
-- Each node is visited exactly once.
foldDag ::
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , HasCallStack
     )
  => (  [a]  -- results for children
     -> node -- current node
     -> a    -- result for current node
     )
  -> DAG nodeId node
  -> a
foldDag f = fst . traverseDag f

-- | Monadic version of 'foldDag'
foldDagM ::
     ( nodeId ~ NodeId node
     , HasChildren node
     , Ord nodeId
     , Show nodeId
     , Monad m
     , HasCallStack
     )
  => (  [a]  -- results for children
     -> node -- current node
     -> m a  -- result for current node
     )
  -> DAG nodeId node
  -> m a
foldDagM f = fmap fst . traverseDagM f

-- | Local type for extending a node type with virtual nodes -- nodes that can
-- have children but no other content
data VirtualNode node nodeId
  = ActualNode node
  | VirtualNode [nodeId]
  deriving (Eq, Show)

instance (HasChildren node, nodeId ~ NodeId node) =>
         HasChildren (VirtualNode node nodeId) where
  type NodeId (VirtualNode node nodeId) = nodeId
  children (ActualNode n) = children n
  children (VirtualNode cs) = cs

handleVirtualError ::
     DagError (VirtualNode node nodeId) nodeId -> DagError node nodeId
handleVirtualError (Cycle c) = Cycle c
handleVirtualError (DanglingEdge cxt e) = case cxt of
  VirtualNode _ -> oops
  ActualNode node -> DanglingEdge node e
handleVirtualError (DuplicateNodes ns) = DuplicateNodes ns

-- | Check for errors in a \"sea of nodes\" that does not have a designated root
-- and may have dangling edges
checkIncompleteDAG ::
     (HasChildren node, Ord nodeId, nodeId ~ NodeId node)
  => Bool -- ^ Allow dangling edges
  -> [(NodeId node, node)]
  -> Either (DagError node nodeId) ()
checkIncompleteDAG allowDangling nodes = first handleVirtualError $
  void $ mkDag fakeRoot $ map (second ActualNode) nodes ++ leaves
  where
    allChildren = ordNub $ concatMap (children . snd) nodes

    -- Virtual root node that has all other nodes as children
    fakeRoot = VirtualNode $ map fst nodes

    -- Virtual leaf nodes for all dangling edges
    dangling =
      [ (ni, VirtualNode [])
      | ni <- allChildren \\ ordNub (map fst nodes)
      ]

    leaves = if allowDangling then dangling else []
