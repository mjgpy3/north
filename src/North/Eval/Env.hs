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
    drain,
    lookupName,
    NameLookupResult (..),
    BuiltInFactor (..),
    FactorDefinition,
) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import North.Eval.DescribedFactor
import North.Eval.Effects
import North.Eval.Errors
import North.Parse.SourceLocation
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
      factors :: Map.Map Text Factor
    , constants :: Map.Map Text Value
    , variables :: Map.Map Text Value
    , effects :: Map.Map Text (DescribedFactor Effect)
    }

lookupName :: EnvState -> Text -> NameLookupResult
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

toNamed :: NameLookupResult -> Maybe Named
toNamed = \case
  FoundConstant {} -> Just NamedConstant
  FoundFactor (NonUserFactor {}) -> Just NamedBuiltInFactor
  FoundFactor (UserFactor {}) -> Just NamedUserFactor
  FoundEffect {} -> Just NamedEffect
  NotFound -> Nothing

pop :: EnvState -> Either EvalError (EnvState, Value)
pop = \case
    envState@EnvState{stack = (v : vs)} -> Right (envState{stack = vs}, v)
    _ -> Left StackUnderflow

push :: Value -> EnvState -> EnvState
push v envState =
    envState{stack = v : stack envState}

drain :: EnvState -> EnvState
drain envState =
    envState{stack = []}

addUserFactor :: SourceLocation Text -> [SourceLocation Value] -> EnvState -> (Env, Either EvalError Value)
addUserFactor name body envState =
  case toNamed $ lookupName envState (located name) of
    Just named -> (State envState, Left $ NameAlreadyDefined (located name) named)
    Nothing -> (State $ envState { factors = Map.insert (located name) (UserFactor body) $ factors envState }, Right Unit)

addVar :: a
addVar = undefined
addConst :: a
addConst = undefined

data Factor
    = UserFactor [SourceLocation Value]
    | NonUserFactor BuiltInFactor

type FactorDefinition = EnvState -> (EnvState, Either EvalError Value)

data BuiltInFactor = BuiltIn (DescribedFactor FactorDefinition)
