module Rhino.Type where

import Rhino.Prelude

import Rhino.AST

data Type
  = BoolType
  | IntegerType
  | FloatType
  | StringType
  deriving (Eq, Show)

data TypeClass
  = NumericClass
  | OtherClass
  deriving (Eq, Show)

litType :: Literal -> Type
litType (Boolean _) = BoolType
litType (Integer _) = IntegerType
litType (Float _)   = FloatType
litType (String _)  = StringType

classOf :: Type -> TypeClass
classOf BoolType    = OtherClass
classOf IntegerType = NumericClass
classOf FloatType   = NumericClass
classOf StringType  = OtherClass
