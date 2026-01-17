module North.Eval.Errors (EvalError (..), Named (..)) where

import Data.Text (Text)
import North.Parse.SourceLocation
import North.Types
import North.Values

data Named
    = NamedConstant
    | NamedUserFactor
    | NamedBuiltInFactor
    | NamedEffect
    deriving (Show, Eq)

data EvalError
    = TypeExpectedButGot (SourceLocation Type) (Type, Value)
    | TypeExpectedButGot' Type (Type, Value)
    | StackUnderflow
    | Located (SourceLocation EvalError)
    | AssertionFailed
    | NameAlreadyDefined Text Named
    | InputPatternHasNoTailButMoreStackRemains
    | PatternMatchFailureNamesNotEqual Char Value Value
    | PatternUnboundRhsName Char
    | AgainCalledOutsideOfFactor (SourceLocation ())
    deriving (Show, Eq)
