{-# LANGUAGE LambdaCase #-}

module North.Eval.Effects
    ( Effect(..)
    ) where

data Effect
  -- Print a newline
  = Cr
  -- Read a file
  | ReadFile
  -- Pop and show whatever's at the top of the stack
  | Print
  -- Pop and show the string at the top of the stack
  | PrintString
