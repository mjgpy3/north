module North.Values (Value(..)) where

import qualified Data.Text as T
import qualified Data.Scientific as Sci

data Value
  = Word T.Text
  | NInt Integer
  | NFloat Sci.Scientific
  | NString T.Text
  | NBool Bool
  | Unit
  deriving (Show, Eq)
