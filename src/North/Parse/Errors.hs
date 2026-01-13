module North.Parse.Errors (ParseError(..)) where

import North.Parse.Tokens
import North.Parse.ParsableTerms

type Side = Either () ()

data ParseError
  = ExpectedNameButGot ParsableTerm
  | IncompleteTopLevelTerm [ParsableTerm]
  | ExpectedValueButGot ParsableTerm
  | CompilerErrorShouldNotExistAtThisStage ParsableTerm
  | UnterminatedFactorDef ParsableTerm

  | PatternMustBeginWithSquare Side ParsableTerm
  | MissingPatternElement Side ParsableTerm
  | UnexpectedCharPattern Side ParsableTerm Char
  | PatternTailMustBeFinal Side ParsableTerm
  | UnterminatedPattern Side ParsableTerm
  | UnexpectedPatternOp ParsableTerm
  | UnexpectedTrailingPatternChars ParsableTerm String
  | ExpectedPatternOpGot ParsableTerm String
  | PatternMustStartWithOpen Side ParsableTerm
  deriving (Show, Eq)
