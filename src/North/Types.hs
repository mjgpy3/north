module North.Types (Type(..)) where

data Type
  = TVar Char
  | TInt
  | TFloat
  | TString
  | TWord
  deriving (Show, Eq)
