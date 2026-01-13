{-# LANGUAGE LambdaCase #-}

module North.Types.TypeOf (typeOf) where

import North.Types
import North.Values

typeOf :: Value -> Type
typeOf = \case
    Word _ -> TWord
    NInt _ -> TInt
    NFloat _ -> TFloat
    NString _ -> TString
    Unit -> TUnit
    NBool _ -> TBool
    Pattern{} -> TPattern
