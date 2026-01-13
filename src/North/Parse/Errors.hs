module North.Parse.Errors (ParseError (..)) where

import North.Parse.ParsableTerms
import North.Parse.Tokens

type Side = Either () ()

data ParseError
    = ExpectedNameButGot ParsableTerm
    | IncompleteTopLevelTerm [ParsableTerm]
    | ExpectedValueButGot ParsableTerm
    | CompilerErrorShouldNotExistAtThisStage ParsableTerm
    | UnterminatedFactorDef ParsableTerm
    | PatternMustBeginWithSquare Side ParsableTerm
    | UnexpectedCharPattern Side ParsableTerm Char
    | UnterminatedPattern Side ParsableTerm
    | UnexpectedPatternOp ParsableTerm
    | UnexpectedTrailingPatternChars ParsableTerm String
    | ExpectedPatternOpGot ParsableTerm String
    | PatternMustStartWithOpen Side ParsableTerm
    deriving (Show, Eq)
