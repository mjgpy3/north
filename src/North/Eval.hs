{-# LANGUAGE LambdaCase #-}

module North.Eval
    ( eval
    , evalMany
    ) where

import North.Eval.Env
import North.Eval.Effects
import North.Eval.Errors
import North.Eval.DescribedFactor
import North.Parse.SourceLocation
import North.TopLevelTerms
import North.Values
import North.Types
import North.Types.TypeOf
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

evalMany :: Env -> [TopLevelTerm] -> IO (Env, Either EvalError Value)
evalMany (RequestedEffect effect envState) terms = do
  (env', result) <- performEffect envState effect
  case result of
    Left err -> pure $ (env', Left err)
    Right _ -> evalMany env' terms
evalMany env terms =
  case terms of
    [] -> pure $ (env, Right Unit)
    term:terms' -> do
      (env', result) <- eval env term
      case result of
        Left err -> pure (env', Left err )
        Right _v -> evalMany env' terms'

evalManyValues :: EnvState -> [SourceLocation Value] -> IO (Env, Either EvalError Value)
evalManyValues envState =
  evalMany (State envState) . fmap TopLevelValue

eval :: Env -> TopLevelTerm -> IO (Env, Either EvalError Value)
eval env term =
  case env of
    RequestedEffect effect envState -> do
      (env', result) <- performEffect envState effect
      case result of
       Left err -> pure $ (env', Left err)
       Right _ -> eval env' term
    State envState ->
      case term of
        TopLevelValue v -> evalValue envState v
        WordDef name body -> pure $ addUserFactor name body envState
        VarDef name -> pure $ addVar name envState
        ConstDef name value -> pure $ addConst name value envState

performEffect :: EnvState -> (SourceLocation Effect) -> IO (Env, Either EvalError Value)
performEffect envState = \case
  SourceLocation {located=Cr} -> do
    putStrLn ""
    pure (State envState, Right Unit)
  loc@(SourceLocation {located=ReadFile}) -> do
    case pop envState of
      Left err -> pure (State envState, Left err)
      Right (envState', NString str) -> do
        text <- TIO.readFile $ T.unpack str
        pure (State $ push (NString text) envState', Right Unit)
      Right (envState', nonString) ->
        pure (State envState', Left $ (fmap (const TString) loc) `TypeExpectedButGot` (typeOf nonString, nonString))
  loc@(SourceLocation {located=Print}) -> do
    case pop envState of
      Left err -> pure (State envState, Left err)
      Right (envState', value) -> do
        print value
        pure (State envState', Right Unit)
  loc@(SourceLocation {located=PrintString}) -> do
    case pop envState of
      Left err -> pure (State envState, Left err)
      Right (envState', NString str) -> do
        putStr $ T.unpack str
        pure (State envState', Right Unit)
      Right (envState', nonString) ->
        pure (State envState', Left $ (fmap (const TString) loc) `TypeExpectedButGot` (typeOf nonString, nonString))

evalValue :: EnvState -> SourceLocation Value -> IO (Env, Either EvalError Value)
evalValue envState loc@(SourceLocation {located=value}) =
  case value of
    -- Simple values go on the stack
    NInt _ -> pure (State $ push value envState, Right Unit)
    NFloat _ -> pure (State $ push value envState, Right Unit)
    NString _ -> pure (State $ push value envState, Right Unit)
    Unit -> pure (State $ push Unit envState, Right Unit)

    Word name ->
      case lookupName envState name of
        -- Known constants get evaluated and pushed to the stack
        FoundConstant constValue -> pure (State $ push constValue envState, Right Unit)
        -- Know user factors get run
        FoundFactor (UserFactor values) ->
          evalManyValues envState values
        FoundFactor (NonUserFactor (BuiltIn (DescribedFactor { factorDefinition=builtIn }))) ->
          pure $ builtIn $ State envState
        -- Know effects get requested
        FoundEffect effect ->
          pure (RequestedEffect (fmap (const effect) loc) envState, Right Unit)
        -- Unknown names go on the stack
        NotFound -> pure (State $ push value envState, Right Unit)
