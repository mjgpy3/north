{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import North
import North.Parse
import Data.Foldable (for_)

main :: IO ()
main = do
  for_ (tokenize ": FOO (hi -- there)\n  DUP * DUP 42;") print
