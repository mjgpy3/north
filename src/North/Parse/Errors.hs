module North.Parse.Errors (ParseError(..)) where

import North.Parse.Tokens
import North.Parse.ParsableTerms

data ParseError
  = ExpectedNameButGot ParsableTerm
  | IncompleteTopLevelTerm [ParsableTerm]
  | ExpectedValueButGot ParsableTerm
  | CompilerErrorShouldNotExistAtThisStage ParsableTerm
  | UnterminatedFactorDef ParsableTerm
  deriving (Show, Eq)
