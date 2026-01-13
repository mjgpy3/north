module North.Values (
  Value(..)
  , StackPattern (..)
  , TransformCheck (..)
  ) where

import qualified Data.Text as T
import qualified Data.Scientific as Sci

-- | Shape of a stack
data StackPattern = StackPattern {
  vertabra :: [Char]
  , hasTail :: Bool
  }
  deriving (Show, Eq)

data TransformCheck
  = Check StackPattern
  | Transform StackPattern StackPattern
  | CheckedTransform StackPattern StackPattern
  deriving (Show, Eq)

data Value
  = Word T.Text
  | NInt Integer
  | NFloat Sci.Scientific
  | NString T.Text
  | NBool Bool
  | Unit
  | Pattern TransformCheck
  deriving (Show, Eq)
