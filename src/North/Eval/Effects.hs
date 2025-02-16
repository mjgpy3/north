{-# LANGUAGE LambdaCase #-}

module North.Eval.Effects
    ( Effect(..)
    ) where

data Effect
  = Cr
  | ReadFile
  | Print
  | PrintString
  | TraceStack
  | Trace
