{-# LANGUAGE OverloadedStrings #-}

module North.BuiltIns (envWithBuiltIns) where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import North.BuiltIns.Effects
import North.BuiltIns.Factors
import North.Eval.Env

envWithBuiltIns :: Env
envWithBuiltIns =
    State
        emptyEnvState
            { factors = Map.fromList $ fmap (second (NonUserFactor . BuiltIn)) $ builtInFactors
            , effects = Map.fromList builtInEffects
            }
