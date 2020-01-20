module Rhino.Load
  ( LoadError (..)
  , moduleFile
  , readModule
  , loadProgam
  , loadAndCheckProgam
  , loadAndCheckProgamWithTarget
  ) where

import Rhino.Prelude

import Control.Exception (throwIO)
import Control.Monad.State (get, modify, execStateT)
import qualified Data.Map.Strict as Map
import System.Directory (findFile)
import System.FilePath ((<.>))

import qualified Text.Megaparsec as Megaparsec

import Data.DAG
import Rhino.AST
import Rhino.Parser
import Rhino.StaticCheck

data LoadError
  = ParseError (Megaparsec.ParseErrorBundle Text Void)
  | StaticError (Contextual StaticError)
  | ModuleNotFound ModulePath
  | ModuleCycle [ModulePath]
  deriving (Show)

instance Exception LoadError where
  displayException (ParseError e) = displayException e
  displayException (StaticError e) = displayException e
  displayException e = show e

-- | Convert a module path to the corresponding file path
moduleFile :: ModulePath -> FilePath
moduleFile (ModulePath path) = toS path <.> "rh"

-- | Read and parse a Rhino module
--
-- May throw a 'LoadError' IO exception.
readModule :: FilePath -> IO Module
readModule file = do
  content <- readFile file
  either (throwIO . ParseError) return $
    Megaparsec.runParser fileParser file content

-- | Make a map of all modules that are transitively imported by the root module
--
-- The function terminates even if the imports are cyclic.
--
-- May throw a 'LoadError' IO exception.
chaseImports
  :: [FilePath] -- ^ Include paths
  -> Module
  -> IO (Map ModulePath Module)
chaseImports includes root = flip execStateT Map.empty $ go Nothing root
  where
    go imp modul = do
      is <- get
      case flip Map.lookup is =<< imp of
        Just _ -> return () -- Already been here
        Nothing -> do
          forM_ imp $ \i -> modify $ Map.insert i modul
          mapM_ go' $ children modul

    go' imp = do
      found <- lift $ findFile includes $ moduleFile imp
      case found of
        Nothing -> lift $ throwIO $ ModuleNotFound imp
        Just file -> do
          modul <- lift $ readModule file
          go (Just imp) modul

liftDagError :: DagError Module ModulePath -> LoadError
liftDagError (Cycle ms) = ModuleCycle ms
-- The following cases should not occur for maps produced by `chaseImports`:
liftDagError (DanglingEdge _ _) = oops
liftDagError (DuplicateNodes _) = oops

-- | Load a Rhino module and all of its transitive imports
--
-- May throw a 'LoadError' IO exception.
loadProgam
  :: [FilePath] -- ^ Include paths
  -> FilePath -- ^ Root module
  -> IO (DAG ModulePath Module)
loadProgam includes rootFile = do
  root <- readModule rootFile
  is <- chaseImports includes root
  either (throwIO . liftDagError) return $ mkDag root $ Map.toList is

-- | Load a Rhino module and all of its transitive imports
--
-- May throw a 'LoadError' IO exception.
loadAndCheckProgam
  :: [FilePath] -- ^ Include paths
  -> FilePath -- ^ Root module
  -> IO (DAG ModulePath (Annotated Module (StaticEnv, StaticEnv)))
loadAndCheckProgam includes rootFile = do
  mods <- loadProgam includes rootFile
  either (throwIO . StaticError) return $ checkProgram mods

-- | Load a Rhino module and all of its transitive imports
--
-- May throw a 'LoadError' IO exception.
loadAndCheckProgamWithTarget
  :: [FilePath] -- ^ Include paths
  -> FilePath -- ^ Root module
  -> Identifier -- ^ Variable holding the result
  -> IO (DAG ModulePath (Annotated Module (StaticEnv, StaticEnv)))
loadAndCheckProgamWithTarget includes rootFile result = do
  mods <- loadProgam includes rootFile
  either (throwIO . StaticError) return $ checkProgramWithTarget mods result
