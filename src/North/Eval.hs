{-# LANGUAGE LambdaCase #-}

module North.Eval (
    eval,
    evalMany,
) where

import Data.Bifunctor (Bifunctor, first, second)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import North.Eval.DescribedFactor
import North.Eval.Effects
import North.Eval.Env
import North.Eval.Errors
import North.Parse.SourceLocation
import North.TopLevelTerms
import North.Types
import North.Types.TypeOf
import North.Values

evalMany :: Env -> [TopLevelTerm] -> IO (Env, Either EvalError Value)
evalMany (RequestedEffect effect envState) terms = do
    (env', result) <- performEffect envState effect
    case result of
        Left err -> pure $ (env', Left err)
        Right _ -> evalMany env' terms
evalMany env terms =
    case terms of
        [] -> pure $ (env, Right Unit)
        term : terms' -> do
            (env', result) <- eval env term
            case result of
                Left err -> pure (env', Left err)
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
performEffect envState loc@(SourceLocation{located = eff}) =
    addLocationToError loc $ case eff of
        Cr -> do
            putStrLn ""
            pure (State envState, Right Unit)
        ReadFile -> do
            case pop envState of
                Left err -> pure (State envState, Left err)
                Right (envState', NString str) -> do
                    text <- TIO.readFile $ T.unpack str
                    pure (State $ push (NString text) envState', Right Unit)
                Right (envState', nonString) ->
                    pure (State envState', Left $ (fmap (const TString) loc) `TypeExpectedButGot` (typeOf nonString, nonString))
        Print -> do
            case pop envState of
                Left err -> pure (State envState, Left err)
                Right (envState', value) -> do
                    print value
                    pure (State envState', Right Unit)
        PrintString -> do
            case pop envState of
                Left err -> pure (State envState, Left err)
                Right (envState', NString str) -> do
                    putStr $ T.unpack str
                    pure (State envState', Right Unit)
                Right (envState', nonString) ->
                    pure (State envState', Left $ (fmap (const TString) loc) `TypeExpectedButGot` (typeOf nonString, nonString))
        Assert -> do
            case pop envState of
                Left err -> pure (State envState, Left err)
                Right (envState', NBool True) ->
                    pure (State envState', Right Unit)
                Right (envState', NBool False) -> do
                    pure (State envState', Left AssertionFailed)
                Right (envState', nonBool) ->
                    pure (State envState', Left $ (fmap (const TBool) loc) `TypeExpectedButGot` (typeOf nonBool, nonBool))
        Trace -> do
            putStrLn $ "TRACE:" <> T.unpack (formatLineColumn loc)
            case envState of
                EnvState{stack = []} -> do
                    putStrLn "  <empty-stack>"
                    pure (State envState, Right Unit)
                EnvState{stack = (value : _)} -> do
                    putStr "  "
                    print value
                    pure (State envState, Right Unit)
        TraceStack -> do
            putStrLn $ "TRACE STACK:" <> T.unpack (formatLineColumn loc)
            for_ (stack envState) $ \value -> do
                putStr "  "
                print value
            putStrLn "<end-of-stack>"
            pure (State envState, Right Unit)

evalValue :: EnvState -> SourceLocation Value -> IO (Env, Either EvalError Value)
evalValue envState loc@(SourceLocation{located = value}) =
    addLocationToError loc $ case value of
        -- Simple values go on the stack
        NInt _ -> pure (State $ push value envState, Right Unit)
        NFloat _ -> pure (State $ push value envState, Right Unit)
        NString _ -> pure (State $ push value envState, Right Unit)
        NBool _ -> pure (State $ push value envState, Right Unit)
        Unit -> pure (State $ push Unit envState, Right Unit)
        Pattern pat -> pure $ evalPattern envState loc pat
        Word name ->
            case lookupName envState name of
                -- Known constants get evaluated and pushed to the stack
                FoundConstant constValue -> pure (State $ push constValue envState, Right Unit)
                -- Know user factors get run
                FoundFactor (UserFactor values) ->
                    evalManyValues envState values
                FoundFactor (NonUserFactor (BuiltIn (DescribedFactor{factorDefinition = builtIn}))) ->
                    pure $ first State $ builtIn envState
                -- Know effects get requested
                FoundEffect effect ->
                    pure (RequestedEffect (fmap (const effect) loc) envState, Right Unit)
                -- Unknown names go on the stack
                NotFound -> pure (State $ push value envState, Right Unit)

evalPattern :: EnvState -> SourceLocation Value -> TransformCheck -> (Env, Either EvalError Value)
evalPattern envState _loc = \case
    Check sp -> (State $ push (NBool $ evalCheck Map.empty (stack envState) sp) envState, Right Unit)
    Transform{} -> error "TODO"
    CheckedTransform{} -> error "TODO"
  where
    evalCheck _ [] StackPattern{vertabra = [], hasTail = False} = True
    evalCheck _ _ StackPattern{vertabra = [], hasTail = True} = True
    evalCheck _ (_ : _) StackPattern{vertabra = [], hasTail = False} = False
    evalCheck _ [] StackPattern{vertabra = _ : _} = False
    evalCheck names (v : vs) sp@(StackPattern{vertabra = n : ns}) =
        case Map.lookup n names of
            Nothing -> evalCheck (Map.insert n v names) vs sp{vertabra = ns}
            Just known ->
                if known == v
                    then evalCheck (Map.insert n v names) vs sp{vertabra = ns}
                    else False

addLocationToError :: (Functor f, Bifunctor p1, Bifunctor p2) => SourceLocation a1 -> f (p1 a2 (p2 EvalError c)) -> f (p1 a2 (p2 EvalError c))
addLocationToError loc = fmap (second (first (\err -> Located $ fmap (const err) loc)))
