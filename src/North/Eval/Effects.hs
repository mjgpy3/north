{-# LANGUAGE LambdaCase #-}

module North.Eval.Effects
    ( Effect(..)
    ) where

data Effect
  -- Print a newline
  = Cr
