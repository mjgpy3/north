{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import North
import North.Parse
import Data.Foldable (for_)

main :: IO ()
main = do
  print (parse ": FOO (hi -- there)\n  DUP * DUP 42 \"Here is a string with escaped quotes \"\"\";")
