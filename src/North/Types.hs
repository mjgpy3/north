module North.Types (Type(..)) where

data Type
  = TVar Char
  | TInt
  | TFloat
  | TNum
  | TString
  | TWord
  | TUnit
  deriving (Show, Eq)
