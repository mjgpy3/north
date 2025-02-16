module North.Parse.ParsableTerms
    ( ParsableTerm(..)
    , RawTerm(..)
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as Sci
import North.Parse.SourceLocation

newtype ParsableTerm = ParsableTerm  { unTerm :: SourceLocation RawTerm }
  deriving (Show, Eq)

data RawTerm =
  IdentTerm T.Text
  | ColonTerm
  | SemicolonTerm
  | IntTerm Integer
  | FloatTerm Sci.Scientific
  | StringTerm T.Text
  | BoolTerm Bool
  -- Too much lift ATM
  -- | Comment T.Text
  | VarTerm
  | ConstTerm
  deriving (Show, Eq)
