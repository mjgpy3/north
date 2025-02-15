module North.Eval.Errors (EvalError(..)) where

import North.Parse.SourceLocation

import North.Types
import North.Values

data EvalError
  = TypeExpectedButGot (SourceLocation Type) (Type, Value)
  deriving (Show, Eq)
