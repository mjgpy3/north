module North.Parse.Tokens
    ( Token(..)
    , RawToken(..)
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as Sci
import North.Parse.SourceLocation

newtype Token = Token { unToken :: SourceLocation RawToken }
  deriving (Show, Eq)

data RawToken =
  Ident T.Text
  | OpenParen
  | CloseParen
  | Backslash
  | Colon
  | Semicolon
  -- Storing for string literals later
  | Whitespace Char
  | IntLiteral Integer
  | FloatLiteral Sci.Scientific
  | Var
  | Const
  deriving (Show, Eq)
