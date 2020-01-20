{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rhino.AST
  ( SourcePos (..)
  , sourcePosPretty
  , module Rhino.AST
  ) where

import Rhino.Prelude
import qualified Prelude

import qualified Data.Set as Set

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific
import Text.Megaparsec (SourcePos (..), sourcePosPretty)

import Data.DAG

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving (Eq, Ord, Hashable, IsString)

-- | Shows without the newtype wrapper
instance Show Identifier where
  show = show . unIdentifier

deriving instance Aeson.ToJSONKey Identifier
deriving instance Aeson.FromJSONKey Identifier

data Literal
  = Boolean !Bool
  | Integer !Integer
  | Float   !Double
  | String  !Text
  deriving (Eq, Show)

instance Aeson.ToJSON Literal where
  toJSON (Boolean b) = Aeson.Bool b
  toJSON (Integer i) = Aeson.Number $ fromInteger i
  toJSON (Float f)   = Aeson.Number $ Scientific.fromFloatDigits f
  toJSON (String s)  = Aeson.String s

instance Aeson.FromJSON Literal where
  parseJSON (Aeson.Bool b) = return $ Boolean b
  parseJSON (Aeson.Number n) = return $ case Scientific.floatingOrInteger n of
    Left f -> Float f
    Right i -> Integer i
  parseJSON (Aeson.String s) = return $ String s
  parseJSON v = fail $ "Unsupported JSON value:\n" ++ show v

data BinOp
  = Plus
  | Minus
  | Times
  | Div
  deriving (Eq, Show)

newtype ModulePath = ModulePath {unModulePath :: Text }
  deriving (Eq, Ord, Hashable, IsString)

-- | Shows without the newtype wrapper
instance Show ModulePath where
  show = show . unModulePath

data Scope
  = Public
  | Private
  deriving (Eq, Show)

data Import = Import
  { importLoc   :: SourcePos
  , importScope :: Scope
  , importPath  :: ModulePath
  } deriving (Eq, Show)

data Input = Input
  { inputLoc   :: SourcePos
  , inputScope :: Scope
  , inputName  :: Identifier
  , inputType  :: Maybe Identifier
  , inputLabel :: Maybe Text
  } deriving (Eq, Show)

data Expression
  = Variable Identifier
  | Literal Literal
  | BinOp BinOp Expression Expression
  | FunCall Identifier [Expression]
  deriving (Eq, Show)

data LocalDef = LocalDef
  { lhs :: Identifier
  , rhs :: Expression
  } deriving (Eq, Show)

data Definition = Definition
  { defLoc    :: SourcePos
  , defScope  :: Scope
  , defName   :: Identifier
  , defArgs   :: [Identifier]
  , defBody   :: [LocalDef]
  , defResult :: Expression
  } deriving (Eq, Show)

data Declaration
  = InputDecl Input
  | DefDecl Definition
  deriving (Eq, Show)

data Module = Module
  { moduleLoc :: SourcePos
  , imports   :: [Import]
  , program   :: [Declaration]
  } deriving (Eq, Show)

instance HasChildren Module where
  type NodeId Module = ModulePath
  children = map importPath . imports

class Located a where
  location :: a -> SourcePos

instance Located Import where
  location = importLoc

instance Located Input where
  location = inputLoc

instance Located Definition where
  location = defLoc

instance Located Declaration where
  location (InputDecl i) = location i
  location (DefDecl d)   = location d

instance Located Module where
  location = moduleLoc

class Scoped a where
  scope :: a -> Scope

instance Scoped Import where
  scope = importScope

instance Scoped Input where
  scope = inputScope

instance Scoped LocalDef where
  scope _ = Private

instance Scoped Definition where
  scope = defScope

instance Scoped Declaration where
  scope (InputDecl i) = scope i
  scope (DefDecl d)   = scope d

class Named a where
  nameOf :: a -> Identifier

instance Named Input where
  nameOf = inputName

instance Named LocalDef where
  nameOf = lhs

instance Named Definition where
  nameOf = defName

instance Named Declaration where
  nameOf (InputDecl i) = nameOf i
  nameOf (DefDecl d)   = nameOf d

newtype Arity = Arity {unArity :: Int}
  deriving (Eq, Num)

instance Show Arity where
  show = show . unArity

class Parameterized a where
  arity :: a -> Arity

instance Parameterized Input where
  arity _ = 0

instance Parameterized LocalDef where
  arity _ = 0

instance Parameterized Definition where
  arity Definition {..} = Arity $ length defArgs

instance Parameterized Declaration where
  arity (InputDecl i) = arity i
  arity (DefDecl d)   = arity d

class HasVars a where
  freeVars :: a -> Set Identifier

instance HasVars Expression where
  freeVars = go
    where
      go (Variable v)   = Set.singleton v
      go (Literal _)    = Set.empty
      go (BinOp _ a b)  = go a <> go b
      go (FunCall _ as) = foldMap go as

instance HasVars Definition where
  freeVars Definition {..} = Set.difference
    (foldr' local (freeVars defResult) defBody)
    (Set.fromList defArgs)
    where
      local LocalDef {..} vs = Set.delete lhs vs <> freeVars rhs

instance HasChildren Definition where
  type NodeId Definition = Identifier
  children = Set.toList . freeVars

instance HasChildren Declaration where
  type NodeId Declaration = Identifier
  children (InputDecl _) = []
  children (DefDecl d) = children d
