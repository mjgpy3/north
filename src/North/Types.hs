module North.Types (Type(..)) where

data Type
  = TVar Char
  | TInt
  | TFloat
  | TString
  | TWord
  | TUnit
  deriving (Show, Eq)
