module Rhino.Evaluate
  ( TypeError (..)
  , InputEnv
  , evaluate
  ) where

import Rhino.Prelude

import Control.Exception (throw)
import qualified Data.Map.Strict as Map

import Data.DAG
import Rhino.AST
import Rhino.Error
import Rhino.StaticCheck
import Rhino.Type
import Rhino.Utils

data TypeError
  = ClassMismatch (Mismatch TypeClass Type)
  deriving (Eq, Show)

instance Exception TypeError

data RHS
  = Val !Literal
  | Closure EvalEnv Definition
  deriving (Eq, Show)

type EvalEnv = Map Identifier RHS

type InputEnv = Map Identifier Literal

declare :: InputEnv -> EvalEnv -> Declaration -> RHS
declare ienv _ (InputDecl Input {..}) = Val $ lookupChecked inputName ienv
declare _ env (DefDecl d) = Closure env d

buildEvalEnv ::
     InputEnv
  -> DAG ModulePath (Annotated Module (StaticEnv, StaticEnv))
  -> EvalEnv
buildEvalEnv inpEnv = foldDag $
  \envs (Annotated (locStaticEnv, _) _) ->
    let -- Combined imported eval environment
        impEvalEnv = Map.unions envs
          -- Static check ensures that imported environments are mergeable

        -- Local definitions
        locDefs = declare inpEnv locEvalEnv <$> locStaticEnv

        -- Local eval environment
        locEvalEnv = Map.union locDefs impEvalEnv -- left-biased

     in locEvalEnv
          -- Static check ensures that the knot terminates (no cyclic
          -- definitions)

numBinOpTypeError :: Literal -> Literal -> a
numBinOpTypeError a b
  | classOf ta /= NumericClass =
    throw $ ClassMismatch $ Mismatch NumericClass ta
  | classOf tb /= NumericClass =
    throw $ ClassMismatch $ Mismatch NumericClass tb
  | otherwise = oops
  where
    ta = litType a
    tb = litType b

numOp ::
     (forall a. Num a => a -> a -> a)
  -> Literal
  -> Literal
  -> Literal
numOp op (Integer a) (Integer b) = Integer $ op a b
numOp op (Integer a) (Float b) = Float $ op (fromInteger a) b
numOp op (Float a) (Integer b) = Float $ op a (fromInteger b)
numOp op (Float a) (Float b) = Float $ op a b
numOp _ a b = numBinOpTypeError a b

fracOp ::
     (forall a. Fractional a => a -> a -> a)
  -> Literal
  -> Literal
  -> Literal
fracOp op (Integer a) (Integer b) = Float $ op (fromInteger a) (fromInteger b)
fracOp op (Integer a) (Float b)   = Float $ op (fromInteger a) b
fracOp op (Float a)   (Integer b) = Float $ op a (fromInteger b)
fracOp op (Float a)   (Float b)   = Float $ op a b
fracOp _ a b = numBinOpTypeError a b

evalBinOp :: BinOp -> Literal -> Literal -> Literal
evalBinOp Plus  = numOp  (+)
evalBinOp Minus = numOp  (-)
evalBinOp Times = numOp  (*)
evalBinOp Div   = fracOp (/)

evalExp :: EvalEnv -> Expression -> Literal
evalExp env = go
  where
--     go (Variable v) = case lookupChecked v env of
    go (Variable v) = case Map.lookup v env of
      Just (Val lit) -> lit
      Just (Closure env' decl) -> evalDef env' decl []
      Nothing -> error $ show (v, Map.keys env)
    go (Literal lit) = lit
    go (BinOp op a b) = evalBinOp op (go a) (go b)
    go (FunCall f as) = case lookupChecked f env of
      Val lit | null as -> lit
      Closure env' decl -> evalDef env' decl $ map go as
      _ -> oops

bindValue :: EvalEnv -> (Identifier, Literal) -> EvalEnv
bindValue env (v, a) = Map.insert v (Val a) env

bindLocalDef :: EvalEnv -> LocalDef -> EvalEnv
bindLocalDef env LocalDef {..} =
  Map.insert lhs (Val $ evalExp env rhs) env

evalDef :: EvalEnv -> Definition -> [Literal] -> Literal
evalDef env Definition {..} as = evalExp env'' defResult
  where
    env'  = foldl' bindValue env $ zipEven defArgs as
    env'' = foldl' bindLocalDef env' $ defBody

evaluate ::
     InputEnv
  -> DAG ModulePath (Annotated Module (StaticEnv, StaticEnv))
  -> Identifier -- ^ Variable holding the result
  -> Literal
evaluate ienv modules result = evalExp env $ Variable result
  where
    env = buildEvalEnv ienv modules
