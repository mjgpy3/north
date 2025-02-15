module North.TopLevelTerms
    ( TopLevelTerm(..)
    ) where

import qualified Data.Text as T
import North.Values
import North.Parse.SourceLocation

data TopLevelTerm
  = TopLevelValue (SourceLocation Value)
  | WordDef (SourceLocation T.Text) [SourceLocation Value]
  | VarDef (SourceLocation T.Text)
  | ConstDef (SourceLocation T.Text) (SourceLocation Value)
  deriving (Show, Eq)
