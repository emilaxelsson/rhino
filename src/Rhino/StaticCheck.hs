module Rhino.StaticCheck
  ( StaticError (..)
  , Contextual (..)
  , StaticEnv
  , checkProgram
  ) where

import Rhino.Prelude

import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.DAG
import Rhino.AST
import Rhino.Error
import Rhino.Utils

data StaticError
  = IncorrectRedefs [Identifier]
  | ImportConflict (Declaration, Import) (Declaration, Import)
  | DefinitionCycle [Identifier]
  | DuplicateDefinitions [Identifier]
  | VariableNotInScope Identifier
  | ArityError Identifier (Mismatch Arity Arity)
  | DuplicatedArgNames [Identifier]
  deriving (Show)

instance Exception StaticError

data Contextual a = Context (Maybe Declaration) a
  deriving (Show)

instance Exception e => Exception (Contextual e) where
  displayException (Context cxt a) = case cxt of
    Nothing -> displayException a
    Just decl ->
      let inTheDecl = case decl of
            InputDecl _ -> "In the input declaration"
            DefDecl _   -> "In the definition of"
       in unlines
            [ sourcePosPretty (location decl)
            , inTheDecl ++ " '" ++ toS (unIdentifier $ nameOf decl) ++ "':"
            , displayException a
            ]



--------------------------------------------------------------------------------
-- * Checking validity of declarations and variable references
--------------------------------------------------------------------------------

type StaticEnv = Map Identifier Declaration

-- | Check that a module's 'redef' definition actually redefine symbols that
-- would otherwise be imported
--
-- Returns the set of redefined symbols.
checkRedefs ::
     [(Import, StaticEnv)]
  -> Module
  -> Either (Contextual StaticError) (Set Identifier)
checkRedefs is Module {..}
  | null nonRedefs = return redefs
  | otherwise = throwError $ Context Nothing $ IncorrectRedefs nonRedefs
  where
    importedSyms = foldMap (Map.keysSet . snd) is
    redefs =
      Set.fromList [defName | DefDecl Definition {redef = True, ..} <- program]
    nonRedefs = Set.toList $ Set.difference redefs importedSyms
      -- Definitions marked as `redef` that aren't actually *re*defining
      -- anything

-- | Select between alternative import statements
--
-- The first one is preferred, unless only the second one is a public import.
resolveImport :: Import -> Import -> Import
resolveImport i j
  | Public <- scope i = i
  | Public <- scope j = j
  | otherwise = i

-- | Resolve two imported declarations for the same identifier
--
-- The reason for pairing the declarations with their imports is two-fold:
--
-- 1. To get more informative error messages
-- 2. To be able to tell whether or not the resolved declaration comes from a
--    public import (i.e. should be reexported)
resolveDecl ::
     (Declaration, Import)
  -> (Declaration, Import)
  -> Either (Contextual StaticError) (Declaration, Import)
resolveDecl (a, i) (b, j)
  | location a == location b = return (a, resolveImport i j)
resolveDecl (a@InputDecl{}, i) (InputDecl{}, j) = return (a, resolveImport i j)
  -- Inputs are identified by name, regardless of where they're declared. We
  -- don't care about metadata here, because it is propagated using a different
  -- mechanism: input metadata is always exported, whether or not the input
  -- symbol is.
resolveDecl a b = throwError $ Context Nothing $ ImportConflict a b

-- | Merge a list of imports and return a mapping from imported symbols to
-- declarations
mergeImports ::
     [(Import, StaticEnv)]
       -- ^ Imports paired with the imported module's exported environment
  -> Map Identifier (NonEmpty (Declaration, Import))
       -- ^ Each imported symbol mapped to its declarations
mergeImports is =
  Map.unionsWith (<>) [fmap (\d -> pure (d, i)) env | (i, env) <- is]

-- | Process the result from 'mergeImports' by resolving identifiers mapping to
-- multiple declarations
resolveMergedImports ::
     Map Identifier (NonEmpty (Declaration, Import))
  -> Either (Contextual StaticError) (Map Identifier (Declaration, Import))
resolveMergedImports = traverse $ foldM1 resolveDecl

liftDagError :: DagError Declaration Identifier -> Contextual StaticError
liftDagError (Cycle vs)           = Context Nothing $ DefinitionCycle vs
liftDagError (DuplicateNodes vs)  = Context Nothing $ DuplicateDefinitions vs
liftDagError (DanglingEdge cxt v) = Context (Just cxt) $ VariableNotInScope v

mkInput :: Declaration -> Input
mkInput decl = Input
  { inputLoc   = location decl
  , inputScope = scope decl
  , inputName  = nameOf decl
  , inputType  = Nothing
  , inputLabel = Nothing
  }

-- | Check that the module defines a valid non-cyclic network of definitions and
-- inputs
checkModuleNetwork ::
     StaticEnv -- ^ From combined imports
  -> Module
  -> Either (Contextual StaticError) ()
checkModuleNetwork impEnv Module {..} =
  first liftDagError $ checkIncompleteDAG False nodes
  where
    impNodes = [(ni, InputDecl $ mkInput d) | (ni, d) <- Map.toList impEnv]
      -- Turn imported declarations to fake inputs so that they don't have any
      -- out-going edges. This is only done temporarily for checking. The inputs
      -- are thrown away afterwards.

    nodes = impNodes ++ map (\d -> (nameOf d, d)) program
      -- All definitions in scope in this module. We're intentionally using a
      -- list, so that any duplicates can be discovered by `checkIncompleteDAG`.

-- | This function does several things:
--
-- * Resolves the module's imports
-- * Checks that `redef` is only used to override existing symbols
-- * Checks that the same symbol is not declared multiple times (unless `redef`
--   is used)
-- * Checks that all referenced variables are in scope
-- * Checks that there are no cycles in the definitions
-- * Returns the module's local and exported environments
checkModule ::
     [(Import, StaticEnv)] -- ^ Unresolved imports
  -> Module
  -> Either (Contextual StaticError) (StaticEnv, StaticEnv)
       -- ^ (local, exported)
checkModule is modul = do
  redefs <- checkRedefs is modul
  let is' = map (second $ flip Map.withoutKeys redefs) is
        -- Remove overridden symbols from the imports. Two reasons for this:
        -- 1. To prevent import resolution from complaining about conflicting
        --    imports when those will anyway be overridden
        -- 2. To get them out of the way in the environment passed to
        --    `checkModuleNetwork`. Otherwise they would lead to
        --    'DuplicateDefinitions'.
  importedEnv <- resolveMergedImports $ mergeImports is'
  let importedEnv' = fst <$> importedEnv
  checkModuleNetwork importedEnv' modul
  let localDecls   = Map.fromListWith oops [(nameOf d, d) | d <- program modul]
        -- `checkModuleNetwork` has checked for absense of clashes
      reexports    = Map.mapMaybe viewPublicImport importedEnv
      localExports = Map.filter ((Public ==) . scope) localDecls
      localEnv     = Map.unionWith oops importedEnv' localDecls
      exportedEnv  = Map.unionWith oops reexports localExports
        -- `checkModuleNetwork` sould complain if these maps are not disjoint
  return (localEnv, exportedEnv)
  where
    viewPublicImport (decl, imp) = case scope imp of
      Public -> Just decl
      Private -> Nothing



--------------------------------------------------------------------------------
-- * Checking arity of function calls
--------------------------------------------------------------------------------

type ArityEnv = Map Identifier Arity

checkArityVariable ::
     ArityEnv
  -> Definition
  -> Identifier
  -> Arity -- ^ Number of arguments given
  -> Either (Contextual StaticError) ()
checkArityVariable env cxt v n =
  when (expectedArity /= n) $
  throwError $
  Context (Just $ DefDecl cxt) $ ArityError v $ Mismatch expectedArity n
  where
    expectedArity = lookupChecked v env

checkArityExpr ::
     ArityEnv -> Definition -> Expression -> Either (Contextual StaticError) ()
checkArityExpr env cxt = go
  where
    go (Variable v) = checkArityVariable env cxt v 0
    go (Literal _) = return ()
    go (BinOp _ a b) = go a >> go b
    go (FunCall f as) =
      mapM_ go as >> checkArityVariable env cxt f (Arity $ length as)

checkArityLocalDef ::
     Definition
  -> ArityEnv
  -> LocalDef
  -> Either (Contextual StaticError) ArityEnv
checkArityLocalDef cxt env LocalDef {..} = do
  checkArityExpr env cxt rhs
  return $ Map.insert lhs 0 env

checkArityDecl :: ArityEnv -> Declaration -> Either (Contextual StaticError) ()
checkArityDecl _ (InputDecl Input {..}) = return ()
checkArityDecl env (DefDecl def@Definition {..}) = do
  -- Check for duplicated arguments
  let dupArgs = duplicates defArgs
  unless (null dupArgs) $
    throwError $ Context (Just $ DefDecl def) $ DuplicatedArgNames dupArgs
  -- Make a local environment from outer environment + arguments
  let argEnv = Map.fromList $ zip defArgs $ repeat 0
      env'   = argEnv `Map.union` env -- left-biased
  -- Extend environment with local definitions
  env'' <- foldM (checkArityLocalDef def) env' defBody
  -- Check result
  checkArityExpr env'' def defResult



--------------------------------------------------------------------------------
-- * Combined static check
--------------------------------------------------------------------------------

checkProgram ::
     DAG ModulePath Module
  -> Either (Contextual StaticError)
       ( DAG ModulePath (Annotated Module (StaticEnv, StaticEnv))
       ) -- ^ (local, exported)
checkProgram modules =
  flip mapAnnotatedM modules' $ \impEnvs (Annotated _ modul@Module {..}) -> do
    let impEnvsExp = map snd impEnvs
    (localEnv, exportedEnv) <- checkModule (zipEven imports impEnvsExp) modul
    mapM_ (checkArityDecl (arity <$> localEnv)) program
    return (localEnv, exportedEnv)
  where
    modules' = annotatedDAG () modules