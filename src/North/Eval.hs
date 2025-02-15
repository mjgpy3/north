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

performEffect :: EnvState -> Effect -> IO (Env, Either EvalError Value)
performEffect envState = \case
  Cr -> do
    putStrLn ""
    pure (State envState, Right Unit)

evalValue :: EnvState -> SourceLocation Value -> (Env, Either EvalError Value)
evalValue envState value = undefined
  -- simple values go on the stack
  -- unknown names go on the stack
  -- known constants get evaluated and pushed to the stack
  -- know factors get run (de-reffing variables will be a builtin)
  -- know effects get requested
