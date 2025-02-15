{-# LANGUAGE LambdaCase #-}

module North.Eval.Env
    ( Env(..)
    , EnvState(..)
    , addUserFactor
    , addVar
    , addConst
    , pop
    , push
    ) where

import North.Eval.Effects
import North.Eval.Errors
import North.Types.SloppySignatures
import North.Parse.SourceLocation
import North.Values
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Env
  = RequestedEffect (SourceLocation Effect) EnvState
  | State EnvState

data EnvState = EnvState {
  stack :: [Value]
  , tempStack :: [Value]
  -- TODO , otherStacks :: Map.Map T.Text [Value]
  , factors :: Map.Map T.Text Factor , constants :: Map.Map T.Text Value
  , variables :: Map.Map T.Text Value
  , effects :: Map.Map T.Text Effect
  }

pop :: EnvState -> Either EvalError (EnvState, Value)
pop = undefined

push :: Value -> EnvState -> EnvState
push = undefined

addUserFactor = undefined
addVar = undefined
addConst = undefined

data Factor
  = UserFactor [SourceLocation Value]
  | NonUserFactor BuiltInFactor

data BuiltInFactor = BuiltIn {
   factorDescription :: T.Text
   , factorSloppySignature :: SloppySignature
   , factorDefinition :: Env -> (Env, Either EvalError Value)
   }
