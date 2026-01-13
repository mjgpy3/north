{-# LANGUAGE LambdaCase #-}

module North.Eval.Env (
    Env (..),
    emptyEnvState,
    EnvState (..),
    Factor (..),
    addUserFactor,
    addVar,
    addConst,
    pop,
    push,
    lookupName,
    NameLookupResult (..),
    BuiltInFactor (..),
    FactorDefinition,
) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import North.Eval.DescribedFactor
import North.Eval.Effects
import North.Eval.Errors
import North.Parse.SourceLocation
import North.Types
import North.Types.SloppySignatures
import North.Values

data Env
    = RequestedEffect (SourceLocation Effect) EnvState
    | State EnvState

emptyEnvState :: EnvState
emptyEnvState =
    EnvState
        { stack = []
        , tempStack = []
        , factors = Map.empty
        , constants = Map.empty
        , variables = Map.empty
        , effects = Map.empty
        }

data EnvState = EnvState
    { stack :: [Value]
    , tempStack :: [Value]
    , -- TODO , otherStacks :: Map.Map T.Text [Value]
      factors :: Map.Map T.Text Factor
    , constants :: Map.Map T.Text Value
    , variables :: Map.Map T.Text Value
    , effects :: Map.Map T.Text (DescribedFactor Effect)
    }

lookupName :: EnvState -> T.Text -> NameLookupResult
lookupName env name =
    fromMaybe NotFound $
        (fmap FoundConstant . Map.lookup name . constants) env
            <|> (fmap FoundFactor . Map.lookup name . factors) env
            <|> (fmap (FoundEffect . factorDefinition) . Map.lookup name . effects) env

data NameLookupResult
    = FoundConstant Value
    | FoundFactor Factor
    | FoundEffect Effect
    | NotFound

pop :: EnvState -> Either EvalError (EnvState, Value)
pop = \case
    envState@EnvState{stack = (v : vs)} -> Right (envState{stack = vs}, v)
    _ -> Left StackUnderflow

push :: Value -> EnvState -> EnvState
push v envState =
    envState{stack = v : stack envState}

addUserFactor = undefined
addVar = undefined
addConst = undefined

data Factor
    = UserFactor [SourceLocation Value]
    | NonUserFactor BuiltInFactor

type FactorDefinition = EnvState -> (EnvState, Either EvalError Value)

data BuiltInFactor = BuiltIn (DescribedFactor FactorDefinition)
