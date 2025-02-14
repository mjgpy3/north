module North.Parse.Tokens
    ( Token(..)
    , RawToken(..)
    ) where

import qualified Data.Text as T
import qualified Data.Scientific as Sci

data Token = Token {
  line :: Int
  , column :: Int
  , tok :: RawToken
  }
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
  deriving (Show, Eq)
