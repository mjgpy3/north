{-# LANGUAGE LambdaCase #-}

module North.Eval
    ( eval
    ) where

import North.Eval.Env
import North.Eval.Effects
import North.Eval.Errors
import North.Parse.SourceLocation
import North.TopLevelTerms
import North.Values
import North.Types
import North.Types.TypeOf
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

eval :: Env -> TopLevelTerm -> IO (Env, Either EvalError Value)
eval env term =
  case env of
    RequestedEffect effect envState -> performEffect envState effect
    State envState ->
      case term of
        TopLevelValue v -> pure $ evalValue envState v
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
  loc@(SourceLocation {located=PrintLine}) -> do
    case pop envState of
      Left err -> pure (State envState, Left err)
      Right (envState', NString str) -> do
        putStr $ T.unpack str
        pure (State envState', Right Unit)
      Right (envState', nonString) ->
        pure (State envState', Left $ (fmap (const TString) loc) `TypeExpectedButGot` (typeOf nonString, nonString))

evalValue :: EnvState -> SourceLocation Value -> (Env, Either EvalError Value)
evalValue envState value = undefined
  -- simple values go on the stack
  -- unknown names go on the stack
  -- known constants get evaluated and pushed to the stack
  -- know factors get run (de-reffing variables will be a builtin)
  -- know effects get requested
