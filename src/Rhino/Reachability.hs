module Rhino.Reachability where

import Rhino.Prelude

import qualified Data.Map.Lazy as Map
  -- Must be lazy because we're building the environment using a knot
import Data.Set (Set)
import qualified Data.Set as Set

import Data.DAG
import Rhino.AST
import Rhino.StaticCheck

type Env = Map Identifier (Set Identifier)

reachExp :: Env -> Expression -> Set Identifier
reachExp env = go
  where
    go (Variable v) = fold $ Map.lookup v env
      -- `fold` will return the empty set for failed lookups. Lookup failure is
      -- expected because function arguments are missing from the environment.
      -- (The static checker has already checked that variables are referred to
      -- correctly.)
    go (BinOp _ a b) = go a <> go b
    go (FunCall f as) = foldMap go as <> fold (Map.lookup f env)
    go (Literal _) = Set.empty

bindLocalDef :: Env -> LocalDef -> Env
bindLocalDef env LocalDef {..} = Map.insert lhs (reachExp env rhs) env

reachDecl :: Env -> Declaration -> Set Identifier
reachDecl _ (InputDecl Input {..}) = Set.singleton inputName
reachDecl env (DefDecl Definition {..}) = reachExp env' defResult
  where
    env' = foldl' bindLocalDef env defBody

reachableInputs ::
     DAG ModulePath (Annotated Module (StaticEnv, StaticEnv))
  -> Identifier -- ^ Variable holding the result
  -> Set Identifier
reachableInputs modules result = reachExp localEnv $ Variable result
  where
    (localEnv, _) = flip foldDag modules $
      \envs (Annotated (_, expStaticEnv) modul) ->
        let -- Combined imported reachability environment
            impEnv = Map.unions $ map snd envs
              -- Static check ensures that imported environments are mergeable

            -- Local definitions
            localDecls = Map.fromListWith oops
              [(nameOf d, reachDecl locEnv d) | d <- program modul]

            -- Local eval environment
            locEnv = Map.union localDecls impEnv -- left-biased

         in (locEnv, Map.intersection locEnv expStaticEnv)
