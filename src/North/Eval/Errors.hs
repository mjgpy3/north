module North.Eval.Errors (EvalError(..)) where

import North.Parse.SourceLocation
import North.Types
import North.Values

data EvalError
  = TypeExpectedButGot (SourceLocation Type) (Type, Value)
  | TypeExpectedButGot' Type (Type, Value)
  | StackUnderflow
  | Located (SourceLocation EvalError)
  | AssertionFailed
  deriving (Show, Eq)
