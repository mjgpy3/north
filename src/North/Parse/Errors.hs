module North.Parse.Errors (ParseError(..)) where

import North.Parse.Tokens

data ParseError
  = ExpectedNameButGot Token
  | IncompleteTopLevelTerm [Token]
  | ExpectedValueButGot Token
  | CompilerErrorShouldNotExistAtThisStage Token
  | UnterminatedFactorDef Token
  deriving (Show, Eq)
